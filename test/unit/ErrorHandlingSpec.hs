{-# LANGUAGE OverloadedStrings #-}

module ErrorHandlingSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Data.Text (Text)
import qualified Data.Text as T

import HsJupyter.Runtime.ErrorHandling
import HsJupyter.Runtime.Diagnostics (DiagnosticSeverity(..), RuntimeDiagnostic(..), mkError)
import HsJupyter.Runtime.GHCDiagnostics (GHCError(..), SourceLocation(..))

spec :: Spec
spec = describe "ErrorHandling" $ do
  
  describe "withTimeoutError" $ do
    it "should handle successful operations" $ do
      let operation = "test-operation"
      result <- withTimeoutError 1 operation (return $ Right ("success" :: Text))
      result `shouldBe` Right "success"
    
    it "should handle timeout scenarios" $ do
      let operation = "slow-operation"
      -- Use a very short timeout to force timeout
      result <- withTimeoutError 0 operation (do
        -- This will timeout before completing
        return $ Right ("never-reached" :: Text))
      case result of
        Left (TimeoutError _) -> return ()
        _ -> expectationFailure "Expected timeout error"
  
  describe "withCancellationCheck" $ do
    it "should proceed when not cancelled" $ do
      cancelVar <- newTVarIO False
      let checkCancelled = readTVar cancelVar
      result <- withCancellationCheck checkCancelled "test-op" 
        (return $ Right ("success" :: Text))
      result `shouldBe` Right "success"
    
    it "should return cancellation error when cancelled" $ do
      cancelVar <- newTVarIO True
      let checkCancelled = readTVar cancelVar
      result <- withCancellationCheck checkCancelled "test-op" 
        (return $ Right ("never-reached" :: Text))
      case result of
        Left (CompilationError msg _ _) -> 
          msg `shouldSatisfy` T.isInfixOf "cancelled"
        _ -> expectationFailure "Expected cancellation error"
  
  describe "enrichDiagnostic" $ do
    it "should add suggestions to diagnostic" $ do
      let originalDiag = mkError "Test error"
      let suggestions = ["Try this", "Or this"]
      let enrichedDiag = enrichDiagnostic originalDiag suggestions
      rdSuggestions enrichedDiag `shouldBe` suggestions
    
    it "should preserve existing suggestions" $ do
      let originalDiag = (mkError "Test error") { rdSuggestions = ["Original"] }
      let newSuggestions = ["New suggestion"]
      let enrichedDiag = enrichDiagnostic originalDiag newSuggestions
      rdSuggestions enrichedDiag `shouldBe` ["Original", "New suggestion"]
  
  describe "propagateError" $ do
    it "should convert CompilationError correctly" $ do
      let ghcError = CompilationError "Syntax error" (SourceLocation 1 1 Nothing) ["Fix syntax"]
      let diagnostic = propagateError ghcError
      rdSeverity diagnostic `shouldBe` SeverityError
      rdSummary diagnostic `shouldBe` "Syntax error"
    
    it "should convert TimeoutError correctly" $ do
      let ghcError = TimeoutError 5
      let diagnostic = propagateError ghcError
      rdSeverity diagnostic `shouldBe` SeverityError
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "timed out"
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "5 seconds"
    
    it "should convert ImportError correctly" $ do
      let ghcError = ImportError "Data.List" "Module not found"
      let diagnostic = propagateError ghcError
      rdSeverity diagnostic `shouldBe` SeverityError
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "Import failed"
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "Data.List"
    
    it "should convert RuntimeError correctly" $ do
      let ghcError = RuntimeError "Division by zero"
      let diagnostic = propagateError ghcError
      rdSeverity diagnostic `shouldBe` SeverityError
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "Runtime error"
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "Division by zero"
  
  describe "error constructors" $ do
    it "should create timeout errors with correct timeout value" $ do
      let err = timeoutError 10 "expression-eval"
      case err of
        TimeoutError seconds -> seconds `shouldBe` 10
        _ -> expectationFailure "Expected TimeoutError"
    
    it "should create cancellation errors with operation context" $ do
      let err = cancellationError "import-module"
      case err of
        CompilationError msg _ suggestions -> do
          msg `shouldSatisfy` T.isInfixOf "cancelled"
          msg `shouldSatisfy` T.isInfixOf "import-module"
          suggestions `shouldNotBe` []
        _ -> expectationFailure "Expected CompilationError for cancellation"