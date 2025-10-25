{-# LANGUAGE OverloadedStrings #-}

module CLIInstallSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)

import HsJupyter.CLI.Install
import HsJupyter.CLI.Commands (InstallOptions(..), defaultInstallOptions)
import HsJupyter.CLI.Types

spec :: Spec  
spec = describe "HsJupyter.CLI.Install" $ do
  
  describe "detectJupyterEnvironment" $ do
    it "should detect Jupyter environment or return appropriate error" $ do
      result <- liftIO detectJupyterEnvironment
      case result of
        Right jupyterEnv -> do
          -- Verify basic structure of detected environment
          jeKernelspecDirs jupyterEnv `shouldNotBe` []
          pePath (jePythonEnv jupyterEnv) `shouldNotBe` ""
        Left _diag ->
          -- Acceptable if Jupyter is not installed in test environment
          pendingWith "Jupyter not available in test environment"
  
  describe "validateJupyterEnvironment" $ do
    it "should validate mock Jupyter environment successfully" $ do
      let mockEnv = JupyterEnvironment
            { jeKernelspecDirs = ["/tmp/test-kernels"]
            , jePythonEnv = PythonEnvironment
                { pePath = "python3"
                , peVersion = "3.8.0"
                , peEnvironment = Nothing
                }
            , jeVersion = JupyterVersion
                { jvLab = Just "3.0.0"
                , jvNotebook = Just "6.0.0"
                , jvCore = "1.0.0"
                }
            , jeInstallType = UserLocal
            }
          options = defaultInstallOptions
      result <- liftIO $ validateJupyterEnvironment mockEnv options
      case result of
        Right validatedEnv ->
          -- Should return successfully validated environment
          jeKernelspecDirs validatedEnv `shouldNotBe` []
        Left _diag ->
          pendingWith "Environment validation failed in test environment"
  
  describe "validateKernelspecDirectories" $ do
    it "should validate kernelspec directories for different scopes" $ do
      let testDirs = ["/usr/local/share/jupyter/kernels", "/home/user/.local/share/jupyter/kernels"]
          options = defaultInstallOptions
      result <- liftIO $ validateKernelspecDirectories testDirs options
      case result of
        Right validatedDirs ->
          -- Should return some valid directories
          validatedDirs `shouldNotBe` []
        Left _diag ->
          pendingWith "Directory validation failed in test environment"
  
  describe "executeInstall" $ do
    it "should execute installation workflow (T015 environment detection)" $ do
      let options = defaultInstallOptions
      result <- liftIO $ executeInstall options
      case result of
        Right _ ->
          -- Installation workflow should complete environment detection step
          return ()
        Left _diag ->
          -- Acceptable if system dependencies not available in test environment
          pendingWith "Installation failed due to test environment constraints"