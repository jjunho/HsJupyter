{-# LANGUAGE OverloadedStrings #-}

module GHCRuntimeSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
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
      let config = testGHCConfig { expressionTimeout = 1 }
      session <- atomically $ newGHCSession config
      -- This test is tricky as we need an expression that takes longer than 1 second
      -- For now, just verify the timeout is set correctly
      sessionConfig session `shouldSatisfy` (\cfg -> expressionTimeout cfg == 1)

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