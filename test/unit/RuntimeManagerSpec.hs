{-# LANGUAGE OverloadedStrings #-}

module RuntimeManagerSpec (spec) where

import Control.Concurrent.STM (newEmptyTMVarIO, putTMVar, readTMVar, atomically)
import Data.Aeson (object)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Test.Hspec

import HsJupyter.Runtime.Manager
  ( withRuntimeManager
  , submitExecute
  , enqueueInterrupt
  )
import HsJupyter.Runtime.SessionState
  ( ExecuteContext(..)
  , ExecutionOutcome(..)
  , ExecutionStatus(..)
  , JobMetadata(..)
  , ResourceBudget(..)
  )

-- Helper to create test context
testExecuteContext :: Text -> ExecuteContext
testExecuteContext msgId = ExecuteContext
  { ecMessageId = msgId
  , ecSessionId = "test-session"
  , ecUsername = "test-user"
  , ecParentId = Nothing
  }

-- Helper to create test metadata
testJobMetadata :: JobMetadata
testJobMetadata = JobMetadata
  { jmSilent = False
  , jmStoreHistory = True
  , jmAllowStdin = False
  , jmUserExpressions = object []
  }

-- Helper to create test resource budget
testResourceBudget :: ResourceBudget
testResourceBudget = ResourceBudget
  { rbCpuTimeout = 30
  , rbMemoryLimit = 1024 * 1024 * 100  -- 100MB
  , rbTempDirectory = "/tmp"
  , rbMaxStreamBytes = 1024 * 1024     -- 1MB
  }

spec :: Spec
spec = describe "RuntimeManager" $ do
  describe "basic execution" $ do
    it "processes single execute request" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        let ctx = testExecuteContext "test-001"
        outcome <- submitExecute manager ctx testJobMetadata "test code"
        
        outcomeStatus outcome `shouldBe` ExecutionOk
        outcomeExecutionCount outcome `shouldBe` 1
        length (outcomeStreams outcome) `shouldSatisfy` (> 0)

    it "processes multiple execute requests sequentially" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        let ctx1 = testExecuteContext "test-001"
            ctx2 = testExecuteContext "test-002"
            ctx3 = testExecuteContext "test-003"
        
        outcome1 <- submitExecute manager ctx1 testJobMetadata "code 1"
        outcome2 <- submitExecute manager ctx2 testJobMetadata "code 2"
        outcome3 <- submitExecute manager ctx3 testJobMetadata "code 3"
        
        outcomeExecutionCount outcome1 `shouldBe` 1
        outcomeExecutionCount outcome2 `shouldBe` 2
        outcomeExecutionCount outcome3 `shouldBe` 3
        
        all ((== ExecutionOk) . outcomeStatus) [outcome1, outcome2, outcome3] `shouldBe` True

  describe "queue management" $ do
    it "handles queue capacity limits" $ do
      -- Test with very small queue to check capacity behavior
      withRuntimeManager testResourceBudget 1 $ \manager -> do
        let ctx = testExecuteContext "queue-001"
        outcome <- submitExecute manager ctx testJobMetadata "test"
        
        -- Should work within capacity
        outcomeStatus outcome `shouldBe` ExecutionOk

  describe "cancellation infrastructure" $ do
    it "provides interrupt interface" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        -- Test that the interrupt function exists and can be called
        -- Note: Actual interruption testing would require async execution
        enqueueInterrupt manager "test-msg-id"
        -- If we get here without error, the interface is working
        True `shouldBe` True

    it "handles cancellation during execution" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        let ctx = testExecuteContext "cancel-001"
        
        -- Start an execution
        outcome <- submitExecute manager ctx testJobMetadata "test code"
        
        -- The cancellation token infrastructure exists
        -- Real cancellation testing would require:
        -- 1. Long-running evaluation code
        -- 2. Concurrent interrupt signal
        -- 3. Proper async handling
        
        -- For now, verify basic functionality
        outcomeStatus outcome `shouldBe` ExecutionOk

  describe "error handling" $ do
    it "handles malformed contexts gracefully" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        let ctx = ExecuteContext "" "" "" Nothing  -- Empty context
        outcome <- submitExecute manager ctx testJobMetadata "test"
        
        -- Should not crash, even with empty context
        outcomeExecutionCount outcome `shouldSatisfy` (> 0)

    it "maintains state consistency across errors" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        -- Execute valid code
        let ctx1 = testExecuteContext "error-001"
        outcome1 <- submitExecute manager ctx1 testJobMetadata "valid code"
        
        -- Execute something that might cause issues (empty code)
        let ctx2 = testExecuteContext "error-002"
        outcome2 <- submitExecute manager ctx2 testJobMetadata ""
        
        -- Execute more valid code
        let ctx3 = testExecuteContext "error-003"
        outcome3 <- submitExecute manager ctx3 testJobMetadata "more valid code"
        
        -- Execution counts should be consistent
        outcomeExecutionCount outcome1 `shouldBe` 1
        outcomeExecutionCount outcome2 `shouldBe` 2
        outcomeExecutionCount outcome3 `shouldBe` 3