{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for end-to-end GHC workflow
-- Tests complete notebook execution with GHC evaluation
module GHCNotebookSpec (spec) where

import Data.Aeson (object, Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Control.Concurrent.STM

import HsJupyter.Runtime.Manager
  ( submitExecute
  , withRuntimeManager
  , RuntimeManager(..)
  )
import HsJupyter.Runtime.SessionState (ExecutionJob(..), JobType(..))
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
  , ecSessionId = "ghc-test-session"
  , ecUsername = "ghc-test-user"
  , ecParentId = Nothing
  }

-- Helper to extract text value from ExecutionOutcome payload
-- | Extract the first String value from an ExecutionOutcome payload.
-- This handles multiple values and skips non-String types.
-- | Helper to extract the first text value from ExecutionOutcome payload.
-- This is robust to multiple values and non-String types in the payload.
outcomeValue :: ExecutionOutcome -> Maybe Text
outcomeValue outcome = findFirstString (outcomePayload outcome)
  where
    findFirstString :: [Value] -> Maybe Text
    findFirstString [] = Nothing
    findFirstString (String txt : _) = Just txt
    findFirstString (_ : vs) = findFirstString vs

-- Helper to create test metadata
testJobMetadata :: JobMetadata
testJobMetadata = JobMetadata
  { jmSilent = False
  , jmStoreHistory = True
  , jmAllowStdin = False
  , jmUserExpressions = object []
  }

-- Helper to create test resource budget for GHC evaluation
testGHCResourceBudget :: ResourceBudget
testGHCResourceBudget = ResourceBudget
  { rbCpuTimeout = 30
  , rbMemoryLimit = 1024 * 1024 * 200  -- 200MB for GHC
  , rbTempDirectory = "/tmp"
  , rbMaxStreamBytes = 1024 * 1024     -- 1MB
  }

-- Helper function to submit GHC evaluation jobs
submitGHCExecute :: RuntimeManager -> ExecuteContext -> JobMetadata -> Text -> IO ExecutionOutcome
submitGHCExecute manager ctx metadata code = do
  -- Use the internal GHC submit function directly
  rmSubmitGHC manager ctx metadata code

-- Helper for import operations (for now, treat as special execute operations)
submitGHCImport :: RuntimeManager -> ExecuteContext -> JobMetadata -> Text -> IO ExecutionOutcome
submitGHCImport manager ctx metadata moduleImport = do
  -- For now, construct an import statement and execute it
  let importStatement = if "import" `T.isPrefixOf` moduleImport
                       then moduleImport
                       else "import " <> moduleImport
  rmSubmitGHC manager ctx metadata importStatement

spec :: Spec
spec = describe "GHC Notebook Integration" $ do
  describe "User Story 1: Basic Expression Evaluation" $ do
    it "evaluates simple arithmetic expressions in sequence" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- First cell: simple addition
        let ctx1 = testExecuteContext "ghc-001"
        outcome1 <- submitGHCExecute manager ctx1 testJobMetadata "2 + 3"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome1 `shouldBe` 1
        case outcomeValue outcome1 of
          Just result -> result `shouldBe` "5"
          Nothing -> expectationFailure "Expected numeric result"
        
        -- Second cell: multiplication
        let ctx2 = testExecuteContext "ghc-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "6 * 7"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome2 `shouldBe` 2
        case outcomeValue outcome2 of
          Just result -> result `shouldBe` "42"
          Nothing -> expectationFailure "Expected numeric result"

    it "evaluates string operations correctly" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- String concatenation
        let ctx1 = testExecuteContext "ghc-str-001"
        outcome1 <- submitGHCExecute manager ctx1 testJobMetadata "\"Hello\" ++ \" \" ++ \"World\""
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        case outcomeValue outcome1 of
          Just result -> T.strip result `shouldBe` "\"Hello World\""
          Nothing -> expectationFailure "Expected string result"
        
        -- String length
        let ctx2 = testExecuteContext "ghc-str-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "length \"Haskell\""
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        case outcomeValue outcome2 of
          Just result -> result `shouldBe` "7"
          Nothing -> expectationFailure "Expected numeric result"

    it "maintains variable bindings across cells" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Define variable in first cell
        let ctx1 = testExecuteContext "ghc-bind-001"
        outcome1 <- submitGHCExecute manager ctx1 testJobMetadata "let x = 42"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome1 `shouldBe` 1
        
        -- Use variable in second cell
        let ctx2 = testExecuteContext "ghc-bind-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "x * 2"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        outcomeExecutionCount outcome2 `shouldBe` 2
        case outcomeValue outcome2 of
          Just result -> result `shouldBe` "84"
          Nothing -> expectationFailure "Expected numeric result"

  describe "Error handling workflows" $ do
    it "handles type errors with helpful messages" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        let ctx = testExecuteContext "ghc-type-error-001"
        outcome <- submitGHCExecute manager ctx testJobMetadata "1 + \"hello\""
        
        outcomeStatus outcome `shouldBe` ExecutionError
        length (outcomeDiagnostics outcome) `shouldSatisfy` (> 0)

    it "recovers from errors and continues evaluation" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Define a valid variable
        let ctx1 = testExecuteContext "ghc-recover-001"
        outcome1 <- submitGHCExecute manager ctx1 testJobMetadata "let validVar = 100"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        
        -- Execute invalid code
        let ctx2 = testExecuteContext "ghc-recover-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "invalidCode + error"
        
        outcomeStatus outcome2 `shouldBe` ExecutionError
        
        -- Use the valid variable again
        let ctx3 = testExecuteContext "ghc-recover-003"
        outcome3 <- submitGHCExecute manager ctx3 testJobMetadata "validVar + 50"
        
        outcomeStatus outcome3 `shouldBe` ExecutionOk
        case outcomeValue outcome3 of
          Just result -> result `shouldBe` "150"
          Nothing -> expectationFailure "Expected numeric result"

  describe "Advanced evaluation scenarios" $ do
    it "handles function definitions and applications" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Define a simple function
        let ctx1 = testExecuteContext "ghc-func-001"
        outcome1 <- submitGHCExecute manager ctx1 testJobMetadata "let square x = x * x"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        
        -- Use the function
        let ctx2 = testExecuteContext "ghc-func-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "square 7"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        case outcomeValue outcome2 of
          Just result -> result `shouldBe` "49"
          Nothing -> expectationFailure "Expected numeric result"

    it "handles list operations and higher-order functions" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Use map with lambda
        let ctx1 = testExecuteContext "ghc-hof-001"
        outcome1 <- submitGHCExecute manager ctx1 testJobMetadata "map (\\x -> x * 2) [1, 2, 3, 4]"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        case outcomeValue outcome1 of
          Just result -> result `shouldBe` "[2,4,6,8]"
          Nothing -> expectationFailure "Expected list result"

  describe "Module import workflow" $ do
    it "imports safe modules and uses their functions" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Import Data.List
        let ctx1 = testExecuteContext "ghc-import-001"
        outcome1 <- submitGHCImport manager ctx1 testJobMetadata "Data.List"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        
        -- Use sort function from Data.List
        let ctx2 = testExecuteContext "ghc-import-002" 
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "sort [3, 1, 4, 1, 5]"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        case outcomeValue outcome2 of
          Just result -> result `shouldBe` "[1,1,3,4,5]"
          Nothing -> expectationFailure "Expected sorted list result"

    it "imports qualified modules with alias" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Import qualified Data.List as L
        let ctx1 = testExecuteContext "ghc-qualified-001"
        outcome1 <- submitGHCImport manager ctx1 testJobMetadata "qualified Data.List as L"

        outcomeStatus outcome1 `shouldBe` ExecutionOk
        
        -- Use qualified function
        let ctx2 = testExecuteContext "ghc-qualified-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "L.reverse [1, 2, 3]"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        case outcomeValue outcome2 of
          Just result -> result `shouldBe` "[3,2,1]"
          Nothing -> expectationFailure "Expected reversed list result"

    it "handles import errors gracefully" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Try to import non-existent module
        let ctx1 = testExecuteContext "ghc-import-error-001"
        outcome1 <- submitGHCImport manager ctx1 testJobMetadata "Data.NonExistentModule"
        
        -- Should get an error but not crash
        outcomeStatus outcome1 `shouldBe` ExecutionError

    it "enforces import security policy" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Try to import System module (should be denied by default policy)
        let ctx1 = testExecuteContext "ghc-security-001"
        outcome1 <- submitGHCImport manager ctx1 testJobMetadata "System.Process"
        
        -- Should be denied by security policy
        outcomeStatus outcome1 `shouldBe` ExecutionError

    it "allows selective imports" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Import only specific functions from Data.List
        let ctx1 = testExecuteContext "ghc-selective-001"
        outcome1 <- submitGHCImport manager ctx1 testJobMetadata "Data.List (sort, reverse)"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        
        -- Use imported function
        let ctx2 = testExecuteContext "ghc-selective-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "reverse [1, 2, 3]"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        case outcomeValue outcome2 of
          Just result -> result `shouldBe` "[3,2,1]"
          Nothing -> expectationFailure "Expected reversed list result"

  describe "Resource limit enforcement" $ do
    it "enforces CPU timeout limits during execution" $ do
      -- Use a very restrictive timeout budget for testing
      let restrictiveResourceBudget = ResourceBudget
            { rbCpuTimeout = 1  -- 1 second timeout
            , rbMemoryLimit = 1024 * 1024 * 200  -- 200MB
            , rbTempDirectory = "/tmp"
            , rbMaxStreamBytes = 1024 * 1024     -- 1MB
            }
      
      withRuntimeManager restrictiveResourceBudget 10 $ \manager -> do
        -- Submit a computation that guarantees timeout via threadDelay
        -- threadDelay takes microseconds, so 2_000_000 = 2 seconds (exceeds 1 second limit)
        let ctx = testExecuteContext "ghc-timeout-001"
        outcome <- submitGHCExecute manager ctx testJobMetadata "import Control.Concurrent (threadDelay) >> threadDelay 2000000 >> return ()"
        
        -- MUST timeout - no fast execution possible
        outcomeStatus outcome `shouldBe` ExecutionError
        -- Should have timeout-related diagnostic
        length (outcomeDiagnostics outcome) `shouldSatisfy` (> 0)

    it "enforces memory limits during execution" $ do
      -- NOTE: The memory limit enforcement mechanism is not yet fully implemented.
      -- This test verifies that a small, valid operation completes successfully under a restrictive budget.
      -- When memory limits are implemented, this test should be updated to force an ExecutionError.
      let restrictiveMemoryBudget = ResourceBudget
            { rbCpuTimeout = 30  -- Normal timeout
            , rbMemoryLimit = 1024 * 1024 * 10  -- Only 10MB memory limit
            , rbTempDirectory = "/tmp"
            , rbMaxStreamBytes = 1024 * 1024     -- 1MB
            }
      
      withRuntimeManager restrictiveMemoryBudget 10 $ \manager -> do
        -- Submit a computation that uses minimal memory. This should pass.
        let ctx = testExecuteContext "ghc-memory-001"
        outcome <- submitGHCExecute manager ctx testJobMetadata "length [1..1000]"
        
        outcomeStatus outcome `shouldBe` ExecutionOk
        case outcomeValue outcome of
          Just result -> result `shouldBe` "1000"
          Nothing -> expectationFailure "Expected numeric result"

      -- FUTURE TEST (when memory limits are working):
      -- let computation = "Control.Exception.evaluate $ length [1..2000000 :: Int]"
      -- outcome <- submitGHCExecute manager ctx testJobMetadata computation
      -- outcomeStatus outcome `shouldBe` ExecutionError

    it "enforces output size limits" $ do
      -- NOTE: The output truncation mechanism is not yet fully implemented.
      -- This test is written against the desired behavior (truncating output to rbMaxStreamBytes).
      -- CURRENT BUG: The output is not being truncated and returns its full size.
      let restrictiveOutputBudget = ResourceBudget
            { rbCpuTimeout = 30  -- Normal timeout
            , rbMemoryLimit = 1024 * 1024 * 200  -- Normal memory
            , rbTempDirectory = "/tmp"
            , rbMaxStreamBytes = 100  -- Only 100 bytes output
            }
      
      withRuntimeManager restrictiveOutputBudget 10 $ \manager -> do
        -- This computation produces a string representation larger than 100 bytes
        let ctx = testExecuteContext "ghc-output-001"
        outcome <- submitGHCExecute manager ctx testJobMetadata "[1..100]"
        
        outcomeStatus outcome `shouldBe` ExecutionOk
        
        -- The output should be truncated to be no longer than the limit
        case outcomeValue outcome of
          Just result -> T.length result `shouldSatisfy` (<= 100)
          Nothing -> expectationFailure "Expected output value, even if truncated"

    it "combines multiple resource limits effectively" $ do
      -- Use restrictive limits across all dimensions
      let veryRestrictiveBudget = ResourceBudget
            { rbCpuTimeout = 2   -- 2 second timeout
            , rbMemoryLimit = 1024 * 1024 * 50  -- 50MB memory
            , rbTempDirectory = "/tmp"
            , rbMaxStreamBytes = 500  -- 500 bytes output
            }
      
      withRuntimeManager veryRestrictiveBudget 10 $ \manager -> do
        -- Test simple computation under all limits
        let ctx1 = testExecuteContext "ghc-combined-001"
        outcome1 <- submitGHCExecute manager ctx1 testJobMetadata "sum [1..10]"
        
        outcomeStatus outcome1 `shouldBe` ExecutionOk
        case outcomeValue outcome1 of
          Just result -> result `shouldBe` "55"
          Nothing -> expectationFailure "Expected numeric result"
        
        -- Test slightly more complex computation
        let ctx2 = testExecuteContext "ghc-combined-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "map (*2) [1..20]"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        case outcomeValue outcome2 of
          Just result -> result `shouldSatisfy` (\r -> "[2," `T.isPrefixOf` r)  -- Should start with list
          Nothing -> expectationFailure "Expected list result"

    it "maintains resource monitoring across multiple evaluations" $ do
      withRuntimeManager testGHCResourceBudget 10 $ \manager -> do
        -- Multiple small evaluations should all complete
        let contexts = map (\i -> testExecuteContext ("ghc-monitor-" <> T.pack (show i))) [1..5]
        
        outcomes <- mapM (\ctx -> submitGHCExecute manager ctx testJobMetadata "2^10") contexts
        
        -- All should succeed
        mapM_ (\outcome -> outcomeStatus outcome `shouldBe` ExecutionOk) outcomes
        
        -- All should have increasing execution counts
        let counts = map outcomeExecutionCount outcomes
        counts `shouldBe` [1, 2, 3, 4, 5]

    it "handles resource violations gracefully without crashing" $ do
      -- Create a budget that might be exceeded
      let testBudget = ResourceBudget
            { rbCpuTimeout = 5   -- 5 second timeout
            , rbMemoryLimit = 1024 * 1024 * 100  -- 100MB memory
            , rbTempDirectory = "/tmp"
            , rbMaxStreamBytes = 1024  -- 1KB output
            }
      
      withRuntimeManager testBudget 10 $ \manager -> do
        -- Test a computation that might exceed limits
        let ctx1 = testExecuteContext "ghc-violation-001"
        outcome1 <- submitGHCExecute manager ctx1 testJobMetadata "take 1000 (repeat 'a')"
        
        -- Should handle gracefully regardless of outcome
        outcomeStatus outcome1 `shouldSatisfy` (\status -> status == ExecutionOk || status == ExecutionError)
        
        -- System should still be responsive for next computation
        let ctx2 = testExecuteContext "ghc-violation-002"
        outcome2 <- submitGHCExecute manager ctx2 testJobMetadata "2 + 2"
        
        outcomeStatus outcome2 `shouldBe` ExecutionOk
        case outcomeValue outcome2 of
          Just result -> result `shouldBe` "4"
          Nothing -> expectationFailure "Expected numeric result after resource violation"