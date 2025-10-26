{-# LANGUAGE OverloadedStrings #-}

module CLICommandsSpec where

import Test.Hspec
import Data.Text (Text)

import HsJupyter.CLI.Commands
import HsJupyter.CLI.Types

spec :: Spec  
spec = describe "HsJupyter.CLI.Commands" $ do
  
  describe "parseCommand" $ do
    it "should parse basic install command" $ do
      let args = ["install"]
          result = parseCommand args
      case result of
        Right (InstallCommand globalOpts installOpts) -> do
          goJSON globalOpts `shouldBe` False
          goQuiet globalOpts `shouldBe` False
          goVerbose globalOpts `shouldBe` False
          ioScope installOpts `shouldBe` AutoDetect
        _ -> expectationFailure $ "Expected InstallCommand, got: " ++ show result
    
    it "should parse install command with global options" $ do
      let args = ["install", "--json", "--quiet"]
          result = parseCommand args
      case result of
        Right (InstallCommand globalOpts _) -> do
          goJSON globalOpts `shouldBe` True
          goQuiet globalOpts `shouldBe` True
          goVerbose globalOpts `shouldBe` False
        _ -> expectationFailure $ "Expected InstallCommand with global options"
    
    it "should parse install command with user scope" $ do
      let args = ["install", "--user"]
          result = parseCommand args
      case result of
        Right (InstallCommand _ installOpts) -> do
          ioScope installOpts `shouldBe` UserInstallation
        _ -> expectationFailure "Expected InstallCommand with UserInstallation scope"
    
    it "should parse install command with system scope" $ do
      let args = ["install", "--system"]
          result = parseCommand args
      case result of
        Right (InstallCommand _ installOpts) -> do
          ioScope installOpts `shouldBe` SystemInstallation
        _ -> expectationFailure "Expected InstallCommand with SystemInstallation scope"
    
    it "should parse install command with force option" $ do
      let args = ["install", "--force"]
          result = parseCommand args
      case result of
        Right (InstallCommand _ installOpts) -> do
          ioForceReinstall installOpts `shouldBe` True
        _ -> expectationFailure "Expected InstallCommand with force option"
    
    it "should parse install command with custom display name" $ do
      let args = ["install", "--display-name", "My Haskell"]
          result = parseCommand args
      case result of
        Right (InstallCommand _ installOpts) -> do
          ioDisplayName installOpts `shouldBe` Just "My Haskell"
        _ -> expectationFailure "Expected InstallCommand with custom display name"
    
    it "should parse install command with validation level" $ do
      let args = ["install", "--validation", "full"]
          result = parseCommand args
      case result of
        Right (InstallCommand _ installOpts) -> do
          ioValidationLevel installOpts `shouldBe` FullValidation
        _ -> expectationFailure "Expected InstallCommand with full validation"
    
    it "should parse doctor command" $ do
      let args = ["doctor"]
          result = parseCommand args
      case result of
        Right (DoctorCommand globalOpts _doctorOpts) -> do
          goJSON globalOpts `shouldBe` False
        _ -> expectationFailure "Expected DoctorCommand"
    
    it "should parse uninstall command" $ do
      let args = ["uninstall", "--verbose"]
          result = parseCommand args
      case result of
        Right (UninstallCommand globalOpts _uninstallOpts) -> do
          goVerbose globalOpts `shouldBe` True
        _ -> expectationFailure "Expected UninstallCommand with verbose option"
    
    it "should parse list command" $ do
      let args = ["list"]
          result = parseCommand args
      case result of
        Right (ListCommand _ _) -> return ()
        _ -> expectationFailure "Expected ListCommand"
    
    it "should handle invalid command" $ do
      let args = ["invalid-command"]
          result = parseCommand args
          isLeft (Left _) = True
          isLeft _ = False
      result `shouldSatisfy` isLeft
    
    it "should handle invalid validation level" $ do
      let args = ["install", "--validation", "invalid"]
          result = parseCommand args
          isLeft (Left _) = True
          isLeft _ = False
      result `shouldSatisfy` isLeft
  
  describe "defaultGlobalOptions" $ do
    it "should have sensible defaults" $ do
      let defaults = defaultGlobalOptions
      goJSON defaults `shouldBe` False
      goQuiet defaults `shouldBe` False
      goVerbose defaults `shouldBe` False
  
  describe "defaultInstallOptions" $ do
    it "should have sensible defaults" $ do
      let defaults = defaultInstallOptions
      ioScope defaults `shouldBe` AutoDetect
      ioForceReinstall defaults `shouldBe` False
      ioValidationLevel defaults `shouldBe` BasicValidation
      ioDisplayName defaults `shouldBe` Nothing
      ioGHCPath defaults `shouldBe` Nothing
      ioJupyterDir defaults `shouldBe` Nothing
      ioKernelspecDir defaults `shouldBe` Nothing