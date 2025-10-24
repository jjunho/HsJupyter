# Data Model: GHC Evaluation

**Feature**: 003-ghc-evaluation | **Date**: 2024-10-25

## Core Entities

### GHCSessionState

**Purpose**: Maintains persistent interpreter state across notebook cell executions

**Fields**:

- `interpreterHandle :: Interpreter` - hint library interpreter instance
- `definedBindings :: TVar (Set String)` - thread-safe tracking of defined variables/functions
- `importedModules :: TVar [ModuleName]` - list of successfully imported modules
- `sessionConfig :: GHCConfig` - configuration including timeout values and import policy

**Relationships**:

- One per RuntimeManager session
- Managed by GHCRuntime
- Integrates with existing SessionState from Phase 2

**State Transitions**:

- Created → Active → Reset → Active (session lifecycle)
- Bindings: Empty → Populated → Filtered (on reset)
- Modules: Empty → Imported → Cleared (on reset)

**Validation Rules**:

- definedBindings must be valid Haskell identifiers
- importedModules must pass import policy check
- interpreterHandle must be active (not terminated)

### GHCConfig

**Purpose**: Configuration for GHC evaluation behavior and security policies

**Fields**:

- `expressionTimeout :: Int` - timeout for simple expressions (default: 1 second)
- `compilationTimeout :: Int` - timeout for imports/compilation (default: 5 seconds)
- `computationTimeout :: Int` - timeout for complex computations (default: 10 seconds)
- `importPolicy :: ImportPolicy` - module import security configuration
- `resourceLimits :: ResourceConfig` - integration with existing ResourceGuard

**Relationships**:

- Referenced by GHCSessionState
- Inherits from existing RuntimeConfig pattern
- Used by GHCRuntime for operation timeouts

**Validation Rules**:

- All timeout values must be > 0 and < 300 seconds
- importPolicy must specify default behavior
- resourceLimits must be compatible with ResourceGuard

### ImportPolicy

**Purpose**: Security configuration for controlling module imports

**Fields**:

- `allowedModules :: Set ModuleName` - explicitly allowed modules
- `deniedModules :: Set ModuleName` - explicitly denied modules  
- `defaultPolicy :: ImportDefault` - behavior for unlisted modules (Allow | Deny)
- `systemModulesAllowed :: Bool` - whether System.* modules are permitted

**Relationships**:

- Embedded in GHCConfig
- Consulted by GHCRuntime before import operations

**Validation Rules**:

- allowedModules and deniedModules must be disjoint
- Standard safe modules should be in allowedModules by default
- Unsafe modules (System.Process, Network.*) should be in deniedModules

### GHCEvaluationRequest

**Purpose**: Input structure for GHC evaluation operations

**Fields**:

- `code :: Text` - Haskell code to evaluate
- `requestType :: EvaluationType` - Expression | Declaration | Import
- `sessionId :: SessionId` - identifies the target session
- `timeoutOverride :: Maybe Int` - optional custom timeout

**Relationships**:

- Processed by GHCRuntime
- Maps to existing RuntimeJob pattern
- Results in GHCEvaluationResult

**Validation Rules**:

- code must be non-empty
- requestType must match code content (imports vs expressions vs declarations)
- sessionId must reference valid active session

### GHCEvaluationResult

**Purpose**: Output structure for GHC evaluation results

**Fields**:

- `success :: Bool` - whether evaluation succeeded
- `result :: Maybe Text` - evaluated result for expressions
- `output :: Text` - stdout/stderr output
- `diagnostics :: [RuntimeDiagnostic]` - errors, warnings, info messages
- `bindingsAdded :: [String]` - new variable/function bindings created
- `modulesImported :: [ModuleName]` - new modules successfully imported

**Relationships**:

- Returned by GHCRuntime operations
- Integrates with existing RuntimeResult pattern
- Contains RuntimeDiagnostic from existing system

**Validation Rules**:

- If success=True, diagnostics should contain no Error-level items
- bindingsAdded should only contain valid Haskell identifiers
- result should be present for Expression requests when success=True

### GHCDiagnostic

**Purpose**: GHC-specific diagnostic information extending RuntimeDiagnostic

**Fields**:

- Inherits from `RuntimeDiagnostic` (severity, message, context)
- `ghcErrorType :: Maybe GHCErrorType` - specific GHC error classification
- `location :: Maybe SourceLocation` - line/column information when available
- `suggestions :: [Text]` - helpful suggestions for fixing errors

**Relationships**:

- Extends existing RuntimeDiagnostic system
- Created by mapping hint InterpreterError
- Included in GHCEvaluationResult

**Validation Rules**:

- Must have valid severity level
- location should be present for compilation errors
- suggestions should be actionable and relevant

## Data Flow Diagrams

### Expression Evaluation Flow

```
GHCEvaluationRequest
    ↓ (validate)
GHCRuntime.evaluateExpression
    ↓ (timeout wrapper)
hint.interpret
    ↓ (result processing)
GHCEvaluationResult ← RuntimeDiagnostic
    ↓ (state update)
GHCSessionState.definedBindings
```

### Import Processing Flow

```
GHCEvaluationRequest (Import)
    ↓ (import policy check)
ImportPolicy.allowedModules
    ↓ (if allowed)
hint.loadModules
    ↓ (update state)
GHCSessionState.importedModules
    ↓ (result)
GHCEvaluationResult
```

### Error Handling Flow

```
hint.InterpreterError
    ↓ (mapping)
GHCDiagnostic
    ↓ (enrichment)
RuntimeDiagnostic + location + suggestions
    ↓ (integration)
GHCEvaluationResult.diagnostics
```

## Integration Points

### Phase 2 Runtime Integration

- **SessionState**: Extend existing session management with GHCSessionState
- **RuntimeJob**: GHC evaluation requests follow existing job queue pattern
- **ResourceGuard**: GHC operations monitored by existing resource limits
- **RuntimeDiagnostic**: GHC errors mapped to existing diagnostic system

### STM Concurrency Integration

- **Job Queue**: GHC evaluation jobs submitted to existing STM-based queue
- **Cancellation**: TMVar-based cancellation applied to hint operations
- **State Management**: GHCSessionState uses TVar for thread-safe access
- **Resource Tracking**: STM coordination with ResourceGuard monitoring

### Performance Characteristics

- **Memory**: ~20-40MB hint interpreter + existing baseline
- **Startup**: ~500ms hint initialization within 2s session target
- **Evaluation**: Expression <200ms, Import <2s, Complex <10s
- **Concurrency**: Single hint interpreter per session, STM coordination

## Validation Schema

### Request Validation

```haskell
validateGHCRequest :: GHCEvaluationRequest -> Either ValidationError GHCEvaluationRequest
validateGHCRequest req = do
  validateNonEmpty (code req)
  validateTypeMatch (requestType req) (code req)
  validateSession (sessionId req)
  validateTimeout (timeoutOverride req)
  return req
```

### Policy Validation  

```haskell
validateImportPolicy :: ImportPolicy -> Either ValidationError ImportPolicy
validateImportPolicy policy = do
  validateDisjoint (allowedModules policy) (deniedModules policy)
  validateSafeModules (allowedModules policy)
  return policy
```

### State Consistency

```haskell
validateSessionState :: GHCSessionState -> STM (Either ValidationError ())
validateSessionState state = do
  bindings <- readTVar (definedBindings state)
  modules <- readTVar (importedModules state)
  validateBindingsSyntax bindings
  validateModulesImported modules (interpreterHandle state)
```
