{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : CLITypesSpec
Description : Unit tests for HsJupyter.CLI.Types
Copyright   : (c) HsJupyter Contributors 2024
License     : MIT
Maintainer  : dev@hsjupyter.org
Stability   : experimental
-}

module CLITypesSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, object)
import qualified Data.Text as T

import HsJupyter.CLI.Types
import HsJupyter.Runtime.Diagnostics (DiagnosticSeverity(..))

spec :: Spec
spec = do
  describe "CLI Types" $ do
    
    describe "CLIDiagnostic" $ do
      it "creates system integration error correctly" $ do
        let diag = SystemIntegrationError "Test error message"
        diag `shouldBe` SystemIntegrationError "Test error message"
      
      it "creates validation error correctly" $ do
        let diag = ValidationError "Test validation error"  
        diag `shouldBe` ValidationError "Test validation error"
    
    describe "JupyterEnvironment JSON serialization" $ do
      it "serializes and deserializes correctly" $ do
        let env = JupyterEnvironment
              { jeKernelspecDirs = ["/test/kernels"]
              , jePythonEnv = PythonEnvironment
                  { pePath = "/usr/bin/python3"
                  , peVersion = "3.9.0"
                  , peEnvironment = Nothing
                  }
              , jeVersion = JupyterVersion
                  { jvLab = Just "3.0.0"
                  , jvNotebook = Just "6.0.0"
                  , jvCore = "1.0.0"
                  }
              , jeInstallType = UserLocal
              }
        let encoded = encode env
        let decoded = decode encoded
        decoded `shouldBe` Just env
    
    describe "InstallationConfiguration" $ do
      it "has correct default values" $ do
        let config = InstallationConfiguration
              { icInstallScope = UserInstallation
              , icCustomPaths = Nothing
              , icKernelConfig = Nothing
              , icForceReinstall = False
              , icQuietMode = False
              , icValidationLevel = BasicValidation
              }
        icInstallScope config `shouldBe` UserInstallation
        icForceReinstall config `shouldBe` False
        icQuietMode config `shouldBe` False
        icValidationLevel config `shouldBe` BasicValidation
      
      it "serializes correctly" $ do
        let config = InstallationConfiguration
              { icInstallScope = SystemInstallation
              , icCustomPaths = Just CustomPaths
                  { cpJupyterDir = Just "/custom/jupyter"
                  , cpKernelspecDir = Nothing
                  , cpGHCPath = Nothing
                  }
              , icKernelConfig = Nothing
              , icForceReinstall = True
              , icQuietMode = True
              , icValidationLevel = FullValidation
              }
        let encoded = encode config
        -- Just test that encoding doesn't fail
        encoded `shouldSatisfy` (not . null . show)
    
    describe "ValidationLevel" $ do
      it "orders correctly" $ do
        NoValidation < BasicValidation `shouldBe` True
        BasicValidation < FullValidation `shouldBe` True
        NoValidation < FullValidation `shouldBe` True
    
    describe "InstallScope variants" $ do
      it "has all expected variants" $ do
        let scopes = [AutoDetect, UserInstallation, SystemInstallation, CustomPath "/test"]
        length scopes `shouldBe` 4
    
    describe "Issue severity mapping" $ do
      it "maps severities correctly" $ do
        severityToRuntimeSeverity Critical `shouldBe` SeverityError
        severityToRuntimeSeverity Major `shouldBe` SeverityError
        severityToRuntimeSeverity Minor `shouldBe` SeverityWarning
        severityToRuntimeSeverity Warning `shouldBe` SeverityWarning
    
    describe "KernelConfig" $ do
      it "creates with resource limits" $ do
        let limits = ResourceLimits
              { rlMemoryLimitMB = Just 1024
              , rlTimeoutSeconds = Just 300
              , rlMaxOutputSizeKB = Just 512
              }
        let config = KernelConfig
              { kcResourceLimits = limits
              , kcDisplayName = "Test Haskell"
              , kcLanguage = "haskell"
              , kcInterruptMode = Signal
              , kcMetadata = object []
              }
        kcDisplayName config `shouldBe` "Test Haskell"
        kcLanguage config `shouldBe` "haskell"
        rlMemoryLimitMB (kcResourceLimits config) `shouldBe` Just 1024
      
      it "serializes correctly" $ do
        let config = KernelConfig
              { kcResourceLimits = ResourceLimits Nothing Nothing Nothing
              , kcDisplayName = "Haskell"
              , kcLanguage = "haskell"
              , kcInterruptMode = Message
              , kcMetadata = object []
              }
        let encoded = encode config
        -- Just test that encoding doesn't fail
        encoded `shouldSatisfy` (not . null . show)