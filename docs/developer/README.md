# Developer Guide

This guide helps contributors understand the project structure, runtime core architecture, and development workflows for HsJupyter.

## Getting Started

### Repository Layout

```text
HsJupyter/
├── src/HsJupyter/
│   ├── Bridge/              # ZeroMQ protocol layer (Phase 1)
│   ├── Kernel/              # Core kernel types
│   ├── Router/              # Message routing  
│   └── Runtime/             # Runtime core (Phase 2)
│       ├── SessionState.hs  # Persistent execution state
│       ├── Manager.hs       # STM job queue
│       ├── Evaluation.hs    # Code evaluation engine
│       ├── Diagnostics.hs   # Error reporting
│       ├── ResourceGuard.hs # Resource limits & monitoring
│       └── Telemetry.hs     # Metrics collection
├── test/
│   ├── unit/                # 38+ unit tests
│   └── integration/         # 12+ integration tests
├── specs/                   # Design documents
└── scripts/demo/            # Testing harnesses
```

### Toolchain Requirements

- **GHC 9.12.2+** (via `ghcup install ghc 9.12.2`)
- **Cabal 3.0+** for build management
- **Python 3.8+** with `pyzmq` for integration testing
- **Git** with feature branch workflow

### Quick Setup

```bash
git clone https://github.com/jjunho/HsJupyter.git
cd HsJupyter
git checkout 003-ghc-evaluation  # Latest GHC evaluation implementation

# Build and test
cabal v2-build all
cabal v2-test all
```

### Build Performance Optimization

Due to the **hint library** (GHC API integration), full builds can take several minutes. For faster development iteration:

**⚡ Fast Development Builds:**

```bash
# Library only (5 seconds vs minutes)
cabal build lib:hs-jupyter-kernel -O0

# Quick compilation check
cabal build --dependencies-only

# Specific test suites only
cabal test unit -O0 --test-option="--match=/GHCSession/"
```

**🔧 Build Configuration Optimizations:**

Add to `~/.cabal/config` or project `cabal.project`:

```cabal
-- Performance optimizations
jobs: 4
documentation: False
haddock-all: False
optimization: 1
split-sections: True
```

**🚀 Development Workflow:**

```bash
# 1. Quick compilation check
cabal build lib:hs-jupyter-kernel -O0

# 2. Run targeted tests
cabal test unit -O0 --test-option="--match=/MyModule/"

# 3. Full integration (when needed)
cabal test integration -O0

# 4. Production build (final step)
cabal build  # With optimizations
```

**Why Builds Are Slow:**

- **hint library**: Includes full GHC API (~100MB+ dependencies)
- **Static linking**: Combines all libraries into executable
- **292 dependencies**: Large dependency graph
- **GHC 9.12.2**: Newer versions can be slower

**Performance Tools:**

```bash
# Install ghcid for instant feedback
cabal install ghcid
ghcid --command="cabal repl lib:hs-jupyter-kernel"

# Monitor build times
time cabal build lib:hs-jupyter-kernel -O0
```

## Runtime Core Architecture

## Running the Prototype

- Entry point: `app/KernelMain.hs`
- CLI:
  - `--connection FILE` points to a Jupyter connection JSON (see `scripts/demo/sample-connection.json`).
  - `--log-level Debug|Info|Warn|Error` or env `HSJUPYTER_LOG_LEVEL`.

Example:

```bash
cabal v2-run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Info
```

Relevant modules:

- Kernel: `src/HsJupyter/KernelProcess.hs`, `src/HsJupyter/Kernel/Types.hs`
- Bridge: `src/HsJupyter/Bridge/{JupyterBridge,HeartbeatThread}.hs`, Protocol `{Envelope,Codec}.hs`
- Router: `src/HsJupyter/Router/RequestRouter.hs`
- Runtime: `src/HsJupyter/Runtime/{Manager,GHCSession,GHCRuntime,Evaluation,Diagnostics,Telemetry,ErrorHandling,ResourceGuard,SessionState}.hs`

### Job Queue System

The runtime uses Software Transactional Memory (STM) for thread-safe job management:

```haskell
-- Runtime/Manager.hs
data RuntimeManager = RuntimeManager
  { rmQueue       :: TQueue ExecutionJob
  , rmCapacity    :: Int
  , rmSessionRef  :: TVar RuntimeSessionState  
  , rmJobRegistry :: TVar (Map JobId TMVar ())  -- Cancellation tokens
  }
```

**Key Operations:**

- `submitExecute`: Enqueue code execution with cancellation token
- `enqueueInterrupt`: Cancel running job by ID
- `withRuntimeManager`: Resource-managed lifecycle

### Session State Management

Persistent state across cell executions:

```haskell
-- Runtime/SessionState.hs  
data RuntimeSessionState = RuntimeSessionState
  { rsExecutionCount   :: Int                    -- Incremental counter
  , rsBindings        :: Map Text Text          -- Variable bindings
  , rsModuleArtifacts :: Map Text ModuleArtifact -- Compiled modules
  , rsImports         :: [Text]                 -- Import declarations
  }
```

**State Lifecycle:**

1. Initialize empty state on runtime startup
2. Increment execution count per cell
3. Accumulate bindings and imports
4. Persist across cancellation/errors

### Resource Management

Configurable resource limits with watchdog enforcement:

```haskell
-- Runtime/ResourceGuard.hs
data ResourceLimits = ResourceLimits
  { rcMaxCpuSeconds  :: Double     -- Wall-clock timeout
  , rcMaxMemoryMB    :: Int        -- Memory limit (RSS)
  , rcMaxOutputBytes :: Int        -- Output truncation limit
  , rcCpuMode        :: CpuLimitMode    -- Wall vs User time
  , rcMemoryMode     :: MemoryLimitMode -- RSS vs Virtual
  }
```

**Enforcement Mechanisms:**

- Background monitoring thread with configurable intervals
- RTS statistics integration for memory tracking
- STM-based cancellation propagation
- Output truncation at render time

### Cancellation Infrastructure

TMVar-based cancellation tokens throughout execution pipeline:

```haskell
-- Job submission creates cancellation token
cancelToken <- newEmptyTMVarIO  
let job = ExecutionJob ctx metadata code cancelToken

-- Background thread monitors for cancellation
checkCancellation :: TMVar () -> IO Bool
checkCancellation token = do
  result <- atomically $ tryReadTMVar token
  return $ isJust result
```

**Cancellation Flow:**

1. Client sends `interrupt_request`
2. RequestRouter calls `enqueueInterrupt`
3. Manager marks job's TMVar
4. Evaluation engine checks token periodically
5. Returns `status=abort` on cancellation

## Development Workflow

### Branching Strategy

Follow numbered feature branches as per `AGENTS.md`:

```bash
# Create feature branch
git checkout -b 003-feature-name

# Work in small, meaningful commits
git commit -m "Phase 1: Setup infrastructure"
git commit -m "Phase 2: Implement core types"
git commit -m "Phase 3: Add comprehensive tests"

# Push when ready for review
git push -u origin 003-feature-name
```

### Code Quality Standards

**Haskell Style:**

- Four-space indentation
- `HsJupyter.*` module namespace
- Total functions preferred over partial
- Haddock comments for public APIs

**Testing Requirements:**

- Unit tests for all new modules
- Integration tests for user-facing features
- Property-based testing for pure functions
- Golden tests for protocol compatibility

### Running Tests

```bash
# All tests
cabal v2-test all

# Specific test suites  
cabal v2-test unit                    # 38+ unit tests
cabal v2-test integration            # 12+ integration tests
cabal v2-test unit -t SessionStateSpec  # Individual test file

# With detailed output
cabal v2-test unit --test-show-details=streaming
```

### Performance Profiling

```bash
# Runtime memory profiling
cabal v2-run hs-jupyter-kernel +RTS -s

# Execution time profiling  
cabal v2-run hs-jupyter-kernel +RTS -p

# Heap profiling
cabal v2-run hs-jupyter-kernel +RTS -h -i0.1
```

## Runtime Queue Usage

### Basic Execution

```haskell
import HsJupyter.Runtime.Manager

-- Setup runtime with resource limits
let budget = ResourceBudget
      { rbMaxMemoryMB = 512
      , rbMaxCpuSeconds = 30.0  
      , rbMaxOutputBytes = 1048576
      }

withRuntimeManager budget 5 $ \manager -> do
  let ctx = ExecuteContext "cell-001" 1 (object [])
      metadata = JobMetadata timestamp correlationId
  
  -- Submit for execution
  outcome <- submitExecute manager ctx metadata "let x = 42"
  
  case outcomeStatus outcome of
    ExecutionOk -> putStrLn "Success!"
    ExecutionError -> putStrLn "Failed!"
    ExecutionAbort -> putStrLn "Cancelled!"
```

### Advanced Queue Management

```haskell
-- Custom resource limits
let strictLimits = ResourceLimits
      { rcMaxCpuSeconds = 5.0      -- 5 second timeout
      , rcMaxMemoryMB = 128        -- 128MB limit
      , rcMaxOutputBytes = 10240   -- 10KB output
      , rcCpuMode = CpuUser        -- User CPU time only
      , rcMemoryMode = MemoryResident
      }

-- Higher concurrency for batch processing
withRuntimeManager customBudget 20 $ \manager -> do
  -- Process multiple cells concurrently
  outcomes <- mapConcurrently (submitExecute manager ctx metadata) codes
  return outcomes
```

## Cancellation Flags

### Implementing Cancellation-Aware Code

```haskell
-- In evaluation engine
evaluateWithCancellation :: TMVar () -> Text -> IO (Maybe Text)
evaluateWithCancellation cancelToken code = do
  -- Check cancellation before expensive operation
  cancelled <- checkCancellation cancelToken
  if cancelled 
    then return Nothing
    else do
      -- Perform computation...
      result <- expensiveComputation code
      
      -- Check again after computation
      cancelled' <- checkCancellation cancelToken  
      if cancelled'
        then return Nothing
        else return (Just result)

checkCancellation :: TMVar () -> IO Bool
checkCancellation token = atomically $ do
  result <- tryReadTMVar token
  return $ isJust result
```

### Cancellation Best Practices

1. **Check Early and Often**: Add cancellation checks before expensive operations
2. **Granular Checking**: Check every few hundred milliseconds in loops
3. **Clean Shutdown**: Ensure resources are released on cancellation
4. **State Consistency**: Don't leave session state in inconsistent state

```haskell
-- Good: Cancellation-aware loop
processItems :: TMVar () -> [Item] -> IO [Result]
processItems cancelToken items = go items []
  where
    go [] acc = return (reverse acc)
    go (x:xs) acc = do
      cancelled <- checkCancellation cancelToken
      if cancelled
        then return (reverse acc)  -- Partial results OK
        else do
          result <- processItem x
          go xs (result:acc)
```

## Resource Tuning

### Memory Management

```haskell
-- For memory-intensive workloads
let memoryHeavyLimits = defaultResourceLimits
      { rcMaxMemoryMB = 2048      -- 2GB limit
      , rcMemoryMode = MemoryResident
      }

-- For memory-constrained environments  
let memoryConstrainedLimits = defaultResourceLimits
      { rcMaxMemoryMB = 256       -- 256MB limit
      , rcMemoryMode = MemoryVirtual
      }
```

### CPU Time Limits

```haskell
-- For compute-heavy tasks
let computeHeavyLimits = defaultResourceLimits
      { rcMaxCpuSeconds = 120.0   -- 2 minute limit
      , rcCpuMode = CpuUser       -- User time only
      }

-- For interactive use
let interactiveLimits = defaultResourceLimits
      { rcMaxCpuSeconds = 5.0     -- 5 second limit  
      , rcCpuMode = CpuWall       -- Wall clock time
      }
```

### Output Management

```haskell
-- For data analysis (large outputs)
let dataAnalysisLimits = defaultResourceLimits
      { rcMaxOutputBytes = 10485760  -- 10MB output
      }

-- For embedded use (minimal outputs)
let embeddedLimits = defaultResourceLimits
      { rcMaxOutputBytes = 4096      -- 4KB output
      }
```

### Monitoring Configuration

```haskell
-- Fine-grained monitoring
let preciseConfig = ResourceConfig
      { rgLimits = defaultResourceLimits
      , rgEnforcement = True
      , rgMonitoringInterval = 0.05   -- 50ms checks
      }

-- Coarse-grained monitoring (lower overhead)
let efficientConfig = ResourceConfig  
      { rgLimits = defaultResourceLimits
      , rgEnforcement = True
      , rgMonitoringInterval = 1.0    -- 1 second checks
      }
```

## Troubleshooting

### Common Issues

**Queue Capacity Exceeded:**

```text
RuntimeManagerException: Queue capacity (5) exceeded
```

- Increase queue capacity in `withRuntimeManager`
- Implement backpressure in client code
- Consider batching or rate limiting

**Memory Limit Violations:**

```text
ResourceViolation: MemoryViolation 1024 512
```

- Increase `rcMaxMemoryMB` limit
- Check for memory leaks in evaluation code
- Profile with `+RTS -h` for heap analysis

**Timeout Errors:**

```text
ResourceViolation: TimeoutViolation 35.2 30.0
```

- Increase `rcMaxCpuSeconds` for long computations
- Add cancellation checks in evaluation loops
- Consider breaking work into smaller chunks

### Debugging Techniques

**STM Deadlock Detection:**

```bash
# Run with STM debugging
cabal v2-run hs-jupyter-kernel +RTS -xc
```

**Resource Monitoring:**

```haskell
-- Add custom telemetry
import HsJupyter.Runtime.Telemetry

logResourceUsage :: ResourceGuard -> IO ()
logResourceUsage guard = do
  stats <- getRTSStats  
  emitMetric "memory.allocated" (allocated_bytes stats)
  emitMetric "memory.live" (live_bytes stats)
```

**Cancellation Debugging:**

```haskell
-- Log cancellation events
debugCancellation :: TMVar () -> Text -> IO ()
debugCancellation token context = do
  cancelled <- atomically $ tryReadTMVar token
  case cancelled of
    Just _ -> putStrLn $ "CANCELLED: " <> T.unpack context
    Nothing -> return ()
```

## Performance Guidelines

### Runtime Manager Optimization

- Use appropriate queue capacity (2-10x CPU cores)
- Monitor queue depth to detect bottlenecks
- Consider work-stealing for CPU-bound tasks

### Session State Efficiency  

- Minimize binding storage size
- Lazy evaluation for module artifacts
- Periodic garbage collection of unused bindings

### Resource Guard Tuning

- Balance monitoring frequency vs overhead
- Use appropriate timeout granularity
- Consider async monitoring for high-throughput scenarios

## Status: ✅ Runtime Core Complete

All development infrastructure and runtime functionality is in place:

- ✅ **75+ comprehensive tests** covering all components
- ✅ **STM-based concurrent architecture** with cancellation support  
- ✅ **Resource management** with configurable limits and enforcement
- ✅ **Session state persistence** across cell executions
- ✅ **Protocol integration** maintaining ZeroMQ compatibility
- ✅ **Development tooling** for testing, profiling, and debugging

Ready for production deployment, advanced feature development, or real GHC evaluation integration.
