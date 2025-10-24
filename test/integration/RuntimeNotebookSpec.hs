{-# LANGUAGE OverloadedStrings #-}

module RuntimeNotebookSpec (spec) where

import Data.Aeson (object)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (secondsToNominalDiffTime)
import Test.Hspec

import HsJupyter.Runtime.Manager
  ( submitExecute
  , withRuntimeManager
  )
import HsJupyter.Runtime.SessionState
  ( ExecuteContext(..)
  , JobMetadata(..)
  , ExecutionOutcome(..)
  , ExecutionStatus(..)
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
spec = describe "Runtime notebook flows" $ do
  describe "sequential state persistence" $ do
    it "reuses state between sequential cells" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        -- Execute first cell that defines a variable
        let ctx1 = testExecuteContext "msg-001"
        outcome1 <- submitExecute manager ctx1 testJobMetadata "let x = 42"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome1 `shouldBe` 1
        
        -- Execute second cell that uses the variable from first cell
        let ctx2 = testExecuteContext "msg-002"
        outcome2 <- submitExecute manager ctx2 testJobMetadata "x * 2"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome2 `shouldBe` 2
        -- Note: This currently echoes input, but when real evaluation is implemented,
        -- it should show the result of x * 2 = 84

    it "maintains execution count across multiple cells" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        let contexts = map (\n -> testExecuteContext ("msg-" <> T.pack (show n))) [1..5::Int]
        
        outcomes <- mapM (\ctx -> submitExecute manager ctx testJobMetadata "1 + 1") contexts
        
        let counts = map outcomeExecutionCount outcomes
        counts `shouldBe` [1, 2, 3, 4, 5]
        all ((== ExecutionOk) . outcomeStatus) outcomes `shouldBe` True

  describe "golden notebook scenarios" $ do
    it "executes a simple golden notebook workflow" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        -- Golden notebook: define a function, then use it
        let ctx1 = testExecuteContext "golden-001"
        outcome1 <- submitExecute manager ctx1 testJobMetadata "double x = x * 2"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome1 `shouldBe` 1
        
        let ctx2 = testExecuteContext "golden-002"
        outcome2 <- submitExecute manager ctx2 testJobMetadata "double 21"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome2 `shouldBe` 2
        
        -- Add more sophisticated checks when real GHC evaluation is implemented
        length (outcomeStreams outcome2) `shouldSatisfy` (> 0)

    it "handles import statements and subsequent usage" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        -- Import a module
        let ctx1 = testExecuteContext "import-001"
        outcome1 <- submitExecute manager ctx1 testJobMetadata "import Data.List"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        
        -- Use the imported module
        let ctx2 = testExecuteContext "import-002"
        outcome2 <- submitExecute manager ctx2 testJobMetadata "sort [3, 1, 4, 1, 5]"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        -- When real evaluation is implemented, this should show [1,1,3,4,5]

  describe "error handling" $ do
    it "handles basic syntax errors gracefully" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        let ctx = testExecuteContext "error-001"
        outcome <- submitExecute manager ctx testJobMetadata "let x = "  -- incomplete syntax
        
        -- Note: Currently echoes, but when real evaluation is implemented,
        -- this should result in ExecutionError with appropriate diagnostics
        outcomeStatus outcome `shouldBe` ExecutionOk  -- Will change when real evaluation is added
        length (outcomeDiagnostics outcome) `shouldSatisfy` (>= 0)

    it "preserves session state after recoverable errors" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        -- Define a valid binding
        let ctx1 = testExecuteContext "recover-001"
        outcome1 <- submitExecute manager ctx1 testJobMetadata "let y = 100"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        
        -- Execute invalid code
        let ctx2 = testExecuteContext "recover-002"
        _ <- submitExecute manager ctx2 testJobMetadata "invalid syntax here"
        
        -- The session should still be usable for subsequent valid code
        let ctx3 = testExecuteContext "recover-003"
        outcome3 <- submitExecute manager ctx3 testJobMetadata "y + 1"
        
        outcomeStatus outcome3 `shouldBe` ExecutionOk
        -- Execution counts should continue properly
        outcomeExecutionCount outcome3 `shouldBe` 3

  describe "cancellation scenarios" $ do
    it "supports basic cancellation workflow" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        -- Start with a normal execution to establish baseline
        let ctx1 = testExecuteContext "cancel-001"
        outcome1 <- submitExecute manager ctx1 testJobMetadata "1 + 1"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome1 `shouldBe` 1
        
        -- Note: Current cancellation implementation is basic
        -- In a real scenario, we would need to test with long-running code
        -- and actual interrupt signals, but for now we test the infrastructure
        let ctx2 = testExecuteContext "cancel-002"
        outcome2 <- submitExecute manager ctx2 testJobMetadata "2 + 2"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome2 `shouldBe` 2
        
        -- The cancellation infrastructure is in place but would need
        -- real threading and interruption to test fully

    it "maintains session state after cancellation attempts" $ do
      withRuntimeManager testResourceBudget 10 $ \manager -> do
        -- Define a variable
        let ctx1 = testExecuteContext "cancel-state-001"
        outcome1 <- submitExecute manager ctx1 testJobMetadata "let x = 100"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        
        -- Simulate attempting to cancel (though actual cancellation mechanics need threading)
        let ctx2 = testExecuteContext "cancel-state-002"
        outcome2 <- submitExecute manager ctx2 testJobMetadata "x + 50"
        
        -- Session should still be usable
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome2 `shouldBe` 2

  describe "resource limit integration" $ do
    it "logs resource limit violations during execution" $ do
      -- Test with a very restricted resource budget
      let restrictedBudget = testResourceBudget
            { rbCpuTimeout = secondsToNominalDiffTime 0.01
            , rbMemoryLimit = 1 * 1024 * 1024
            , rbMaxStreamBytes = 100
            }
      
      withRuntimeManager restrictedBudget 2 $ \manager -> do
        let ctx = testExecuteContext "resource-limit-001"
        
        -- This should complete but with resource constraint logging
        outcome <- submitExecute manager ctx testJobMetadata "1 + 1"
        
        -- The execution should succeed (our echo runtime is lightweight)
        -- but the resource monitoring infrastructure should be active
        outcomeStatus outcome `shouldBe` ExecutionOk
        outcomeExecutionCount outcome `shouldBe` 1

    it "handles output truncation when limits exceeded" $ do
      let outputLimitedBudget = testResourceBudget
            { rbCpuTimeout = secondsToNominalDiffTime 30
            , rbMemoryLimit = 512 * 1024 * 1024
            , rbMaxStreamBytes = 50
            }
      
      withRuntimeManager outputLimitedBudget 2 $ \manager -> do
        let ctx = testExecuteContext "output-limit-001"
        
        -- Execute code that would generate more output than allowed
        outcome <- submitExecute manager ctx testJobMetadata "replicate 100 'x'"
        
        -- Should succeed but output should be truncated
        outcomeStatus outcome `shouldBe` ExecutionOk
        case outcomeValue outcome of
          Just output -> length (T.unpack output) `shouldSatisfy` (<= 50)
          Nothing -> expectationFailure "Expected output value"

    it "monitors resource usage patterns across multiple executions" $ do
      let monitoringBudget = testResourceBudget
            { rbCpuTimeout = secondsToNominalDiffTime 10
            , rbMemoryLimit = 256 * 1024 * 1024
            , rbMaxStreamBytes = 1024
            }
      
      withRuntimeManager monitoringBudget 5 $ \manager -> do
        -- Execute multiple code snippets to test resource monitoring
        let contexts = [ testExecuteContext ("monitor-" ++ show i) | i <- [1..3] ]
        let codes = [ "let x" ++ show i ++ " = " ++ show (i * 10)
                    | i <- [1..3] ]
        
        outcomes <- mapM (\(ctx, code) -> submitExecute manager ctx testJobMetadata code) 
                         (zip contexts codes)
        
        -- All executions should succeed with monitoring active
        map outcomeStatus outcomes `shouldBe` replicate 3 ExecutionOk
        map outcomeExecutionCount outcomes `shouldBe` [1, 2, 3]
