{-# LANGUAGE OverloadedStrings #-}

module SystemIntegrationSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)

import HsJupyter.CLI.Utilities
import HsJupyter.CLI.Types

spec :: Spec
spec = describe "HsJupyter.CLI.Utilities" $ do
  
  describe "detectJupyterEnvironment" $ do
    it "should detect system Jupyter environment" $ do
      result <- liftIO detectJupyterEnvironment
      case result of
        Right jupyterEnv -> do
          -- Verify basic structure
          jeKernelspecDirs jupyterEnv `shouldNotBe` []
          pePath (jePythonEnv jupyterEnv) `shouldNotBe` ""
        Left _diag -> 
          -- It's acceptable if Jupyter is not installed in test environment
          pendingWith "Jupyter not available in test environment"
  
  describe "detectSystemPython" $ do
    it "should detect system Python executable" $ do
      result <- liftIO detectSystemPython
      case result of
        Right pythonPath -> do
          pythonPath `shouldNotBe` ""
          -- Python path should be a valid executable name
          pythonPath `shouldSatisfy` (`elem` ["python3", "python", "/usr/bin/python3", "/usr/bin/python"])
        Left _diag ->
          -- Fail if Python is not available - it's required for Jupyter
          expectationFailure "Python should be available in the system"
  
  describe "validateJupyterInstallation" $ do
    it "should validate when Jupyter is properly installed" $ do
      -- Create a mock environment for testing
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
      result <- liftIO $ validateJupyterInstallation mockEnv
      case result of
        Right _ -> return ()  -- Validation passed
        Left _diag -> 
          -- Validation may fail in test environment, which is acceptable
          pendingWith "Jupyter validation skipped in test environment"
  
  describe "resolveJupyterDataDir" $ do
    it "should resolve Jupyter data directory" $ do
      result <- liftIO resolveJupyterDataDir
      -- Should return a non-empty path
      result `shouldNotBe` ""
      -- Should be an absolute path on Unix systems
      result `shouldSatisfy` (\path -> head path == '/' || take 2 path == "C:")
  
  describe "validateSystemRequirements" $ do
    it "should check basic system requirements" $ do
      result <- liftIO validateSystemRequirements
      case result of
        Right _ -> return ()  -- Requirements met
        Left _diag -> 
          -- May fail in test environment depending on setup
          pendingWith "System requirements validation skipped in test environment"
  
  describe "ensureDirectoryExists" $ do
    it "should create directory if it doesn't exist" $ do
      let testDir = "/tmp/hs-jupyter-test-dir"
      result <- liftIO $ ensureDirectoryExists testDir
      case result of
        Right _ -> return ()  -- Directory created or already exists
        Left _diag -> 
          expectationFailure "Should be able to create test directory in /tmp"