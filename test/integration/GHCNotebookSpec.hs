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
outcomeValue :: ExecutionOutcome -> Maybe Text
outcomeValue outcome = case outcomePayload outcome of
  [value] -> case value of
    String txt -> Just txt
    _ -> Nothing
  _ -> Nothing

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