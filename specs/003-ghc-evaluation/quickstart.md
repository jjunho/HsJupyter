# Quickstart: GHC Evaluation

**Feature**: 003-ghc-evaluation | **Date**: 2024-10-25

## Overview

This quickstart guide demonstrates how to implement and test GHC evaluation functionality in HsJupyter. Follow these steps to transform the kernel from an echo server into a functional Haskell REPL.

## Prerequisites

Before starting implementation:

1. **Complete Phase 2**: Ensure all Phase 2 Runtime Core functionality is working
2. **Run existing tests**: Verify all 78 tests pass with `cabal v2-test`
3. **Check dependencies**: Confirm `hint >= 0.9.0` is available in `hs-jupyter-kernel.cabal`
4. **GHC version**: Using GHC 9.12.2 via ghcup

## Implementation Steps

### Step 1: Create Core GHC Modules (30 minutes)

Create the foundational GHC integration modules:

```bash
# Create new module files
touch src/HsJupyter/Runtime/GHCRuntime.hs
touch src/HsJupyter/Runtime/GHCSession.hs  
touch src/HsJupyter/Runtime/GHCDiagnostics.hs

# Create corresponding test files
touch test/unit/GHCRuntimeSpec.hs
touch test/unit/GHCSessionSpec.hs
touch test/unit/GHCDiagnosticsSpec.hs
```

**Start with GHCSession.hs** (simplest, no hint dependencies yet):

```haskell
{-# LANGUAGE OverloadedStrings #-}
module HsJupyter.Runtime.GHCSession where

import Control.Concurrent.STM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

data GHCSessionState = GHCSessionState
  { definedBindings :: TVar (Set String)
  , importedModules :: TVar [String]  -- Start simple, ModuleName later
  , sessionActive :: TVar Bool
  }

-- | Create new empty session state
newGHCSession :: STM GHCSessionState
newGHCSession = do
  bindings <- newTVar Set.empty
  modules <- newTVar []
  active <- newTVar True
  return $ GHCSessionState bindings modules active

-- | Add binding to session
addBinding :: GHCSessionState -> String -> STM ()
addBinding session name = modifyTVar' (definedBindings session) (Set.insert name)

-- | List all current bindings
listBindings :: GHCSessionState -> STM [String]
listBindings session = Set.toList <$> readTVar (definedBindings session)
```

**Test it immediately**:

```haskell
-- test/unit/GHCSessionSpec.hs
module GHCSessionSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import HsJupyter.Runtime.GHCSession

spec :: Spec
spec = describe "GHCSession" $ do
  it "creates empty session" $ do
    session <- atomically newGHCSession
    bindings <- atomically $ listBindings session
    bindings `shouldBe` []
    
  it "adds and lists bindings" $ do
    session <- atomically newGHCSession
    atomically $ addBinding session "x"
    bindings <- atomically $ listBindings session
    bindings `shouldBe` ["x"]
```

Run test: `cabal v2-test --test-options="--match GHCSession"`

### Step 2: Add Basic hint Integration (45 minutes)

**Update GHCRuntime.hs** with minimal hint integration:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module HsJupyter.Runtime.GHCRuntime where

import Language.Haskell.Interpreter
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T

data GHCError 
  = CompilationError Text
  | RuntimeError Text
  deriving (Show, Eq)

-- | Simple expression evaluation (no session state yet)
evaluateExpression :: Text -> IO (Either GHCError Text)
evaluateExpression code = do
  result <- runInterpreter $ do
    setImports ["Prelude"]
    interpret (T.unpack code) (as :: String)
  case result of
    Left err -> return . Left . CompilationError . T.pack . show $ err
    Right val -> return . Right . T.pack $ val
```

**Test basic evaluation**:

```haskell
-- test/unit/GHCRuntimeSpec.hs
module GHCRuntimeSpec (spec) where

import Test.Hspec
import HsJupyter.Runtime.GHCRuntime

spec :: Spec
spec = describe "GHCRuntime" $ do
  it "evaluates simple arithmetic" $ do
    result <- evaluateExpression "2 + 3"
    result `shouldBe` Right "5"
    
  it "handles type errors" $ do
    result <- evaluateExpression "1 + \"hello\""
    case result of
      Left (CompilationError _) -> return ()
      _ -> expectationFailure "Expected compilation error"
```

### Step 3: Integrate with Runtime Manager (30 minutes)

**Extend existing RuntimeManager** to support GHC jobs:

```haskell
-- Add to src/HsJupyter/Runtime/Manager.hs
import qualified HsJupyter.Runtime.GHCRuntime as GHC

-- Extend existing RuntimeJob type
data RuntimeJob
  = EchoJob JobId Text (TMVar RuntimeResult)
  | GHCJob JobId Text (TMVar RuntimeResult)  -- NEW

-- Add GHC job processing to existing processJob function
processJob :: RuntimeJob -> RuntimeM ()
processJob job = case job of
  EchoJob jobId input resultVar -> do
    -- existing echo logic
    
  GHCJob jobId code resultVar -> do  -- NEW
    result <- liftIO $ GHC.evaluateExpression code
    case result of
      Right value -> atomically $ putTMVar resultVar $ RuntimeResult
        { resultSuccess = True
        , resultOutput = value
        , resultDiagnostics = []
        }
      Left err -> atomically $ putTMVar resultVar $ RuntimeResult
        { resultSuccess = False  
        , resultOutput = ""
        , resultDiagnostics = [errorToDiagnostic err]
        }
```

### Step 4: Add Integration Test (20 minutes)

**Create end-to-end test**:

```haskell
-- test/integration/GHCNotebookSpec.hs
module GHCNotebookSpec (spec) where

import Test.Hspec
import HsJupyter.Runtime.Manager
import Control.Concurrent.STM

spec :: Spec
spec = describe "GHC Notebook Integration" $ do
  it "processes GHC evaluation jobs" $ do
    manager <- initializeRuntimeManager defaultRuntimeConfig
    resultVar <- newEmptyTMVarIO
    let job = GHCJob (JobId 1) "reverse \"hello\"" resultVar
    
    submitJob manager job
    result <- atomically $ takeTMVar resultVar
    
    resultSuccess result `shouldBe` True
    resultOutput result `shouldBe` "\"olleh\""
```

## Testing Strategy

### Unit Tests First

Always write failing tests before implementation:

```bash
# Run specific test while developing
cabal v2-test --test-options="--match GHCRuntime"

# Run all tests to ensure no regression
cabal v2-test
```

### Integration Testing

Test complete workflow from job submission to result:

```bash
# Create integration test for full pipeline
cabal v2-test --test-options="--match 'GHC.*Integration'"
```

### Performance Testing

Verify timeout and resource behavior:

```haskell
it "respects evaluation timeouts" $ do
  -- Test infinite loop with timeout
  result <- evaluateExpressionWithTimeout 1 "let loop = loop in loop"
  case result of
    Left (TimeoutError _) -> return ()
    _ -> expectationFailure "Expected timeout error"
```

## Common Issues & Solutions

### Issue 1: hint Module Not Found

**Error**: `Could not find module 'Language.Haskell.Interpreter'`

**Solution**: Check `hs-jupyter-kernel.cabal` contains:

```cabal
build-depends: hint >= 0.9.0
```

### Issue 2: GHC Version Mismatch

**Error**: `hint` package requires different GHC version

**Solution**: Verify GHC version compatibility:

```bash
ghc --version  # Should be 9.12.2
cabal v2-build --dependencies-only
```

### Issue 3: STM Integration Issues

**Error**: `hint` operations block STM transactions

**Solution**: Use `liftIO` to lift hint operations:

```haskell
evaluateInSTM :: Text -> STM (IO (Either GHCError Text))
evaluateInSTM code = return $ evaluateExpression code
```

### Issue 4: Resource Limits Not Applied

**Error**: GHC operations ignore existing resource limits

**Solution**: Wrap hint calls with existing ResourceGuard:

```haskell
import HsJupyter.Runtime.ResourceGuard

evaluateWithLimits :: Text -> ResourceM (Either GHCError Text)
evaluateWithLimits code = withResourceGuard $ evaluateExpression code
```

## Next Steps

After completing this quickstart:

1. **Run full test suite**: Ensure all existing tests pass
2. **Manual testing**: Test basic expressions in actual notebook
3. **Performance validation**: Verify timeout behavior
4. **Error handling**: Test various error scenarios

### Ready for Tasks Phase

Once quickstart validation is complete:

```bash
# Generate task breakdown
/speckit.tasks

# Begin implementation following task list
/speckit.implement
```

### Success Criteria

- [ ] Basic expression evaluation works: `2 + 3` → `5`
- [ ] Error handling works: `1 + "hello"` → meaningful error
- [ ] Integration preserves existing functionality: all Phase 2 tests pass
- [ ] Performance meets targets: simple expressions < 200ms
- [ ] STM integration works: no deadlocks or race conditions

This quickstart provides the foundation for full GHC evaluation implementation while maintaining all existing Phase 2 functionality.
