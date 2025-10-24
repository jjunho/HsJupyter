# GHC Evaluation API Contracts

**Feature**: 003-ghc-evaluation | **Date**: 2024-10-25

## Overview

This document defines the internal API contracts for GHC evaluation integration. These are Haskell module interfaces, not REST/HTTP APIs, as this feature extends the existing kernel runtime.

## Core Module Contracts

### HsJupyter.Runtime.GHCRuntime

**Purpose**: Main interface for GHC-based code evaluation

```haskell
module HsJupyter.Runtime.GHCRuntime
  ( -- * Core evaluation functions
    evaluateExpression
  , evaluateDeclaration  
  , importModule
    
    -- * Session management
  , initializeGHCSession
  , resetGHCSession
  , getSessionBindings
    
    -- * Configuration
  , GHCConfig(..)
  , defaultGHCConfig
  ) where

-- | Evaluate a Haskell expression and return its result
evaluateExpression :: GHCSessionState -> Text -> STM (Either GHCError Text)

-- | Execute a Haskell declaration (variable/function definition)
evaluateDeclaration :: GHCSessionState -> Text -> STM (Either GHCError [String])

-- | Import a Haskell module with security policy checking
importModule :: GHCSessionState -> ModuleName -> STM (Either GHCError ())

-- | Initialize a new GHC session with configuration
initializeGHCSession :: GHCConfig -> STM (Either GHCError GHCSessionState)

-- | Reset session state while preserving interpreter instance
resetGHCSession :: GHCSessionState -> STM ()

-- | Get list of currently defined bindings
getSessionBindings :: GHCSessionState -> STM [String]
```

### HsJupyter.Runtime.GHCSession

**Purpose**: Session state management and persistence

```haskell
module HsJupyter.Runtime.GHCSession
  ( -- * Session state types
    GHCSessionState(..)
  , GHCConfig(..)
  , ImportPolicy(..)
    
    -- * State operations
  , addBinding
  , removeBinding
  , listBindings
  , addImportedModule
  , listImportedModules
    
    -- * Session lifecycle
  , withGHCSession
  , cleanupSession
  ) where

data GHCSessionState = GHCSessionState
  { interpreterHandle :: Interpreter
  , definedBindings :: TVar (Set String)
  , importedModules :: TVar [ModuleName]
  , sessionConfig :: GHCConfig
  }

data GHCConfig = GHCConfig
  { expressionTimeout :: Int
  , compilationTimeout :: Int
  , computationTimeout :: Int
  , importPolicy :: ImportPolicy
  , resourceLimits :: ResourceConfig
  }

-- | Add a new binding to the session state
addBinding :: GHCSessionState -> String -> STM ()

-- | Remove a binding from the session state
removeBinding :: GHCSessionState -> String -> STM ()

-- | Execute action with managed GHC session
withGHCSession :: GHCConfig -> (GHCSessionState -> STM a) -> STM (Either GHCError a)
```

### HsJupyter.Runtime.GHCDiagnostics

**Purpose**: Error handling and diagnostic reporting

```haskell
module HsJupyter.Runtime.GHCDiagnostics
  ( -- * Error types
    GHCError(..)
  , GHCErrorType(..)
  , SourceLocation(..)
    
    -- * Error conversion
  , interpretError
  , ghcErrorToDiagnostic
  , enrichDiagnostic
    
    -- * Suggestion system
  , generateSuggestions
  , commonErrorSuggestions
  ) where

data GHCError
  = CompilationError Text SourceLocation [Text]
  | RuntimeError Text
  | TimeoutError Int
  | ImportError ModuleName Text
  | SecurityError Text
  deriving (Show, Eq)

data SourceLocation = SourceLocation
  { sourceLine :: Int
  , sourceColumn :: Int
  , sourceFile :: Maybe FilePath
  } deriving (Show, Eq)

-- | Convert hint InterpreterError to GHCError
interpretError :: InterpreterError -> GHCError

-- | Convert GHCError to RuntimeDiagnostic for Phase 2 integration
ghcErrorToDiagnostic :: GHCError -> RuntimeDiagnostic

-- | Generate helpful suggestions for common errors
generateSuggestions :: GHCError -> [Text]
```

## Integration Contracts

### Runtime Manager Integration

**Purpose**: Integration with existing Phase 2 RuntimeManager

```haskell
-- Extension to existing HsJupyter.Runtime.Manager
module HsJupyter.Runtime.Manager where

-- | Execute GHC evaluation job (extends existing executeJob)
executeGHCJob :: JobId -> GHCEvaluationRequest -> RuntimeM RuntimeResult

-- | Initialize GHC runtime alongside existing runtime components
initializeGHCRuntime :: RuntimeConfig -> RuntimeM ()

-- | Shutdown GHC runtime and cleanup resources
shutdownGHCRuntime :: RuntimeM ()
```

### Job Queue Integration

**Purpose**: Integration with existing STM-based job queue

```haskell
-- Extension to existing job types
data RuntimeJob
  = EchoJob JobId Text (TMVar RuntimeResult)
  | GHCJob JobId GHCEvaluationRequest (TMVar RuntimeResult)  -- NEW
  | DiagnosticJob JobId DiagnosticRequest (TMVar RuntimeResult)

-- GHC job processing
processGHCJob :: GHCSessionState -> GHCEvaluationRequest -> STM RuntimeResult
```

## Request/Response Contracts

### GHC Evaluation Request

```haskell
data GHCEvaluationRequest = GHCEvaluationRequest
  { reqCode :: Text
  , reqType :: EvaluationType
  , reqSessionId :: SessionId
  , reqTimeout :: Maybe Int
  } deriving (Show, Eq)

data EvaluationType
  = Expression      -- Return computed value
  | Declaration     -- Define variable/function
  | Import         -- Import module
  deriving (Show, Eq)
```

### GHC Evaluation Response

```haskell
data GHCEvaluationResult = GHCEvaluationResult
  { resSuccess :: Bool
  , resResult :: Maybe Text
  , resOutput :: Text
  , resDiagnostics :: [RuntimeDiagnostic]
  , resBindingsAdded :: [String]
  , resModulesImported :: [ModuleName]
  } deriving (Show, Eq)
```

## Security Contracts

### Import Policy Interface

```haskell
module HsJupyter.Runtime.ImportPolicy where

data ImportPolicy = ImportPolicy
  { allowedModules :: Set ModuleName
  , deniedModules :: Set ModuleName
  , defaultPolicy :: ImportDefault
  , systemModulesAllowed :: Bool
  }

data ImportDefault = Allow | Deny deriving (Show, Eq)

-- | Check if module import is allowed by policy
checkImportAllowed :: ImportPolicy -> ModuleName -> Bool

-- | Default safe module configuration
defaultSafePolicy :: ImportPolicy

-- | Default restrictive configuration for production
defaultRestrictivePolicy :: ImportPolicy
```

## Performance Contracts

### Timeout Management

```haskell
module HsJupyter.Runtime.GHCTimeout where

-- | Apply timeout to GHC operation based on type
withGHCTimeout :: EvaluationType -> GHCConfig -> IO a -> IO (Maybe a)

-- | Cancel GHC operation via TMVar signal
cancelGHCOperation :: TMVar () -> IO a -> IO (Either CancelledError a)

-- | Combine timeout and cancellation
withTimeoutAndCancellation :: Int -> TMVar () -> IO a -> IO (Either GHCError a)
```

### Resource Monitoring

```haskell
-- Integration with existing ResourceGuard
monitorGHCResources :: GHCSessionState -> IO a -> IO (Either ResourceError a)

-- Resource limits specific to GHC operations
data GHCResourceLimits = GHCResourceLimits
  { maxMemoryMB :: Int
  , maxCPUTimeSeconds :: Int
  , maxOutputBytes :: Int
  }
```

## Testing Contracts

### Mock Interfaces

```haskell
module HsJupyter.Runtime.GHCRuntime.Mock where

-- | Mock GHC runtime for unit testing
data MockGHCRuntime = MockGHCRuntime
  { mockEvaluationResults :: [Either GHCError Text]
  , mockBindings :: [String]
  , mockImports :: [ModuleName]
  }

-- | Create mock runtime with predefined responses
createMockRuntime :: MockGHCRuntime -> GHCSessionState

-- | Verify mock runtime received expected calls
verifyMockCalls :: MockGHCRuntime -> [GHCEvaluationRequest] -> Bool
```

## Backwards Compatibility

### Phase 2 Compatibility

All existing Phase 2 interfaces remain unchanged:

- `RuntimeManager` continues to handle job dispatch
- `SessionState` is extended, not replaced
- `RuntimeDiagnostic` system is reused for GHC errors
- STM-based job queue accepts new GHCJob type
- ResourceGuard monitoring applies to GHC operations

### Migration Path

1. **Phase 1**: Add GHC modules alongside existing EchoRuntime
2. **Phase 2**: Runtime Manager dispatches to both Echo and GHC based on configuration
3. **Phase 3**: Default to GHC runtime, maintain Echo for testing/fallback
4. **Phase 4**: Deprecate EchoRuntime (future consideration)

The contract ensures zero breaking changes to existing functionality while adding comprehensive GHC evaluation capabilities.
