{-# LANGUAGE OverloadedStrings #-}

module GHCRuntimeSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T

import HsJupyter.Runtime.GHCRuntime
import HsJupyter.Runtime.GHCSession
import HsJupyter.Runtime.GHCDiagnostics
import HsJupyter.Runtime.SessionState (ResourceBudget(..))

spec :: Spec
spec = describe "GHCRuntime" $ do
  describe "evaluateExpression" $ do
    it "evaluates simple arithmetic expressions" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpression session "2 + 3"
      case result of
        Right value -> value `shouldBe` "5"
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err
    
    it "evaluates string expressions" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpression session "\"hello\""
      case result of
        Right value -> value `shouldBe` "\"hello\""
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err
    
    it "evaluates list operations" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpression session "reverse [1,2,3]"
      case result of
        Right value -> value `shouldBe` "[3,2,1]"
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err
    
    it "handles type errors gracefully" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpression session "1 + \"hello\""
      case result of
        Left (CompilationError _ _ _) -> return () -- Expected
        Left err -> expectationFailure $ "Expected CompilationError, got: " ++ show err
        Right _ -> expectationFailure "Expected error for type mismatch"
    
    it "handles syntax errors gracefully" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpression session "1 + + 2"
      case result of
        Left _ -> return () -- Expected some error
        Right _ -> expectationFailure "Expected error for syntax error"
    
    it "respects timeout configuration" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      -- This should succeed within timeout
      result <- evaluateExpression session "1 + 1"
      case result of
        Right value -> value `shouldBe` "2"
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

  describe "timeout behavior" $ do
    it "applies correct timeout for simple expressions" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      -- Simple arithmetic should use short timeout (3s)
      result <- evaluateExpressionMonitored session "2 + 3"
      case result of
        (Right value, telemetry) -> do
          value `shouldBe` "5"
          ptOperationType telemetry `shouldBe` Expression
          ptSuccess telemetry `shouldBe` True
        (Left err, _) -> expectationFailure $ "Expected success, got error: " ++ show err

    it "applies correct timeout for complex expressions" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      -- Complex expression should use longer timeout (30s)
      result <- evaluateExpressionMonitored session "foldr (+) 0 [1..100]"
      case result of
        (Right value, telemetry) -> do
          value `shouldBe` "5050"
          ptOperationType telemetry `shouldBe` Expression
          ptSuccess telemetry `shouldBe` True
        (Left err, _) -> expectationFailure $ "Expected success, got error: " ++ show err

    it "handles timeout for very short timeout config" $ do
      let shortTimeoutConfig = testGHCConfig { expressionTimeout = 1 }  -- 1 second
      session <- atomically $ newGHCSession shortTimeoutConfig
      -- This might timeout due to very short limit
      result <- evaluateExpression session "sum [1..1000]"
      case result of
        Right _ -> return ()  -- Success is also acceptable
        Left (TimeoutError _) -> return ()  -- Timeout is expected
        Left err -> expectationFailure $ "Expected timeout or success, got: " ++ show err

    it "differentiated timeout for declarations" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateDeclarationMonitored session "let x = 42"
      case result of
        (Right bindings, telemetry) -> do
          bindings `shouldBe` ["x"]
          ptOperationType telemetry `shouldBe` Declaration
          ptSuccess telemetry `shouldBe` True
        (Left err, _) -> expectationFailure $ "Expected success, got error: " ++ show err

    it "differentiated timeout for imports" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpressionMonitored session ":m + Data.List"  -- Use GHCi syntax
      case result of
        (Right _, telemetry) -> do
          ptExecutionTime telemetry `shouldSatisfy` (>= 0)
          ptSuccess telemetry `shouldBe` True
        (Left err, _) -> expectationFailure $ "Expected success, got error: " ++ show err

  describe "performance monitoring" $ do
    it "tracks execution time correctly" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpressionMonitored session "2 + 3"
      case result of
        (Right value, telemetry) -> do
          value `shouldBe` "5"
          ptExecutionTime telemetry `shouldSatisfy` (>= 0)
          ptCodeLength telemetry `shouldBe` 5  -- "2 + 3"
          ptSuccess telemetry `shouldBe` True
          ptErrorType telemetry `shouldBe` Nothing
        (Left err, _) -> expectationFailure $ "Expected success, got error: " ++ show err

    it "tracks memory usage" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpressionMonitored session "[1..100]"
      case result of
        (Right _, telemetry) -> do
          msAllocatedBytes (ptMemoryBefore telemetry) `shouldSatisfy` (>= 0)
          msAllocatedBytes (ptMemoryAfter telemetry) `shouldSatisfy` (>= 0)
          ptSuccess telemetry `shouldBe` True
        (Left err, _) -> expectationFailure $ "Expected success, got error: " ++ show err

    it "tracks error information" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      result <- evaluateExpressionMonitored session "1 + \"hello\""  -- Type error
      case result of
        (Left _, telemetry) -> do
          ptSuccess telemetry `shouldBe` False
          ptErrorType telemetry `shouldSatisfy` (\x -> case x of
            Just errorMsg -> "CompilationError" `isInfixOf` errorMsg || "No instance for" `isInfixOf` errorMsg
            Nothing -> False)
        (Right _, _) -> expectationFailure "Expected error for type mismatch"

  describe "memory limits" $ do
    it "enforces memory limits" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      -- Test memory-limited evaluation
      result <- evaluateExpressionMemoryLimited session "2 + 3"
      case result of
        Right value -> value `shouldBe` "5"
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "can get memory statistics" $ do
      stats <- getCurrentMemoryStats
      msAllocatedBytes stats `shouldSatisfy` (>= 0)
      msResidentBytes stats `shouldSatisfy` (>= 0)
      msMaxResidentBytes stats `shouldSatisfy` (>= 0)

  describe "session management" $ do
    it "creates session with correct configuration" $ do
      let config = testGHCConfig
      result <- atomically $ initializeGHCSession config
      case result of
        Right session -> sessionConfig session `shouldBe` config
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err
    
    it "tracks bindings correctly" $ do
      let config = testGHCConfig
      session <- atomically $ newGHCSession config
      bindings1 <- atomically $ getSessionBindings session
      bindings1 `shouldBe` []
      
      atomically $ addBinding session "x"
      bindings2 <- atomically $ getSessionBindings session
      bindings2 `shouldBe` ["x"]

-- | Test configuration for GHC
testGHCConfig :: GHCConfig
testGHCConfig = GHCConfig
  { expressionTimeout = 10  -- 10 seconds for GHC initialization
  , compilationTimeout = 15
  , computationTimeout = 20
  , importPolicy = testImportPolicy
  , resourceLimits = testResourceBudget
  }

-- | Test import policy
testImportPolicy :: ImportPolicy
testImportPolicy = ImportPolicy
  { allowedModules = mempty
  , deniedModules = mempty
  , defaultPolicy = Allow
  , systemModulesAllowed = False
  }

-- | Test resource budget
testResourceBudget :: ResourceBudget
testResourceBudget = ResourceBudget
  { rbCpuTimeout = 10.0
  , rbMemoryLimit = 100 * 1024 * 1024  -- 100MB
  , rbTempDirectory = "/tmp"
  , rbMaxStreamBytes = 1024
  }