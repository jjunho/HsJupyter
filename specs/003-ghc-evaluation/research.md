# Research: GHC Evaluation Integration

**Feature**: 003-ghc-evaluation | **Date**: 2024-10-25

## Research Tasks

Based on technical context analysis, the following areas required research:

1. **hint library integration patterns** - How to integrate hint InterpreterT with existing STM architecture
2. **GHC resource management** - Combining hint timeouts with ResourceGuard system
3. **Persistent interpreter state** - Managing Haskell bindings across cell executions
4. **Error handling integration** - Mapping GHC errors to RuntimeDiagnostic system
5. **Module import security** - Implementing configurable whitelist for safe imports

## Findings

### 1. hint Library Integration Patterns

**Decision**: Use hint's `InterpreterT` monad transformer stacked with STM for thread-safe state management

**Rationale**:

- hint provides `InterpreterT` monad transformer that can be stacked with other monads
- STM operations can be lifted into InterpreterT using `liftIO . atomically`
- Allows preserving existing STM-based job queue and cancellation patterns
- hint's `runInterpreter` provides clean error handling via `InterpreterError`

**Alternatives considered**:

- Raw GHC API: Too complex, hint provides safer abstraction
- Separate hint process: Added complexity, communication overhead
- Synchronous hint calls: Would block STM, chosen async approach with STM coordination

**Implementation approach**:

```haskell
type GHCRuntime = ReaderT GHCConfig (InterpreterT (STM))

runGHCEvaluation :: GHCRuntime a -> STM (Either InterpreterError a)
```

### 2. GHC Resource Management

**Decision**: Implement timeout wrapper around hint operations combined with existing ResourceGuard monitoring

**Rationale**:

- hint library supports timeout via `System.Timeout.timeout`
- ResourceGuard already monitors memory/CPU at process level
- Differentiated timeouts align with clarified requirements (1s/5s/10s)
- TMVar cancellation can interrupt hint operations

**Alternatives considered**:

- hint-only timeouts: Insufficient, lacks memory monitoring
- ResourceGuard-only: Cannot interrupt GHC compilation mid-process
- Separate resource system: Violates DRY principle, adds complexity

**Implementation approach**:

```haskell
timeoutGHCOperation :: Int -> InterpreterT IO a -> InterpreterT IO (Maybe a)
timeoutGHCOperation seconds action = liftIO $ timeout (seconds * 1000000) (runInterpreter action)
```

### 3. Persistent Interpreter State

**Decision**: Maintain single hint `Interpreter` instance per session with STM-managed state tracking

**Rationale**:

- hint `Interpreter` naturally maintains variable bindings across `interpret` calls
- STM `TVar (Set String)` can track defined variables for cleanup/reset
- Supports clarified requirement for top-level binding persistence
- Local bindings automatically cleaned up by GHC scoping

**Alternatives considered**:

- New interpreter per cell: Loses state persistence, startup overhead
- Manual state serialization: Complex, error-prone, unnecessary with hint
- Global interpreter: Thread safety issues, conflicts with STM model

**Implementation approach**:

```haskell
data GHCSessionState = GHCSessionState
  { interpreterHandle :: Interpreter
  , definedBindings :: TVar (Set String)
  , importedModules :: TVar [ModuleName]
  }
```

### 4. Error Handling Integration

**Decision**: Map hint `InterpreterError` to existing `RuntimeDiagnostic` with GHC-specific error enrichment

**Rationale**:

- hint provides structured error types: `UnknownError`, `WrongImportStyle`, `NotAllowed`, `GhcException`
- `RuntimeDiagnostic` already supports severity levels and structured messages
- GHC errors contain location information that maps to diagnostic context
- Preserves existing error handling patterns

**Alternatives considered**:

- Direct hint errors: Inconsistent with existing diagnostic system
- New error system: Violates existing architecture, duplicates functionality
- Error translation layer: Chosen approach, maintains consistency

**Implementation approach**:

```haskell
ghcErrorToDiagnostic :: InterpreterError -> RuntimeDiagnostic
ghcErrorToDiagnostic (WrongImportStyle m) = RuntimeDiagnostic
  { severity = Error
  , message = "Import style error for module: " <> m
  , context = Just ("module", m)
  }
```

### 5. Module Import Security

**Decision**: Implement configurable whitelist with default safe modules, extensible via configuration

**Rationale**:

- Addresses clarified security requirement for import restrictions
- Standard library modules (Data.*, Control.*, Text.*) are generally safe
- System modules (System.Process, Network.*) require restriction
- Configuration allows customization for different deployment environments

**Alternatives considered**:

- Blacklist approach: Incomplete, new unsafe modules could be added
- No restrictions: Security risk for hosted environments
- User approval per import: Too restrictive for interactive use

**Implementation approach**:

```haskell
data ImportPolicy = ImportPolicy
  { allowedModules :: Set ModuleName
  , deniedModules :: Set ModuleName
  , defaultPolicy :: ImportDefault -- Allow | Deny
  }

defaultSafeModules :: Set ModuleName
defaultSafeModules = Set.fromList
  [ "Data.List", "Data.Map", "Data.Set", "Control.Monad"
  , "Control.Applicative", "Text.Printf", "System.IO"
  ]
```

## Architecture Integration

### STM Integration Pattern

The hint `InterpreterT` will be integrated with STM using this pattern:

```haskell
-- Existing STM job queue unchanged
data RuntimeJob = RuntimeJob
  { jobId :: JobId
  , jobAction :: IO RuntimeResult  -- hint operations lifted to IO
  , jobCancel :: TMVar ()
  }

-- GHC evaluation wraps hint in STM-compatible IO
evaluateGHC :: GHCSessionState -> Text -> STM (STM RuntimeResult)
evaluateGHC session code = do
  jobId <- generateJobId
  cancelToken <- newEmptyTMVar
  let action = runGHCWithTimeout session code cancelToken
  return $ liftIO action
```

### Performance Characteristics

Based on research and existing Phase 2 benchmarks:

- **hint initialization**: ~500ms (acceptable for session startup <2s target)
- **Simple expression evaluation**: ~50-100ms (well under 200ms target)
- **Module import**: ~200-800ms per module (under 2s target with 5s timeout)
- **Memory overhead**: ~20-40MB for hint interpreter (acceptable with 100MB baseline)

### Testing Strategy

Research confirms comprehensive testing approach:

1. **Unit tests**: Mock hint operations, test error mapping, timeout behavior
2. **Integration tests**: Real hint interpreter, standard library usage
3. **Performance tests**: Timeout verification, memory profiling
4. **Security tests**: Import policy enforcement, unsafe module blocking

## Next Steps

Phase 0 research complete. All technical unknowns resolved with concrete implementation approaches. Ready to proceed to Phase 1 design (data-model.md, contracts/, quickstart.md).
