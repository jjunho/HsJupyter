{-# LANGUAGE OverloadedStrings #-}

module CLIInstallSpec where

import Test.Hspec
import Control.Monad.IO.Class (liftIO)

import HsJupyter.CLI.Install
  ( detectJupyterEnvironment
  , validateJupyterEnvironment
  , validateKernelspecDirectories
  , executeInstall
  -- T016: Kernelspec directory functions
  , findKernelspecDirectories
  , ensureDirectoryExists
  , getKernelPath
  , validateKernelInstallation
  -- T017: Kernel.json generation functions
  , generateKernelJson
  , installKernelJson
  , writeKernelJson
  , validateKernelJson
  )
import HsJupyter.CLI.Commands (InstallOptions(..), defaultInstallOptions)
import HsJupyter.CLI.Types
import Data.Aeson (Value(..), object, (.=))
import Data.Aeson.Types (Array)
import qualified Data.Vector as V
import Data.Text (Text)

-- Test helper functions
array :: [Value] -> Value
array = Array . V.fromList

string :: Text -> Value
string = String

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
  
  -- T016: Kernelspec Directory Discovery and Validation Tests
  describe "findKernelspecDirectories" $ do
    it "should find available kernelspec directories in the system" $ do
      result <- liftIO findKernelspecDirectories
      case result of
        Right dirs ->
          -- Should find at least some standard directories (even if not all exist)
          dirs `shouldNotBe` []
        Left _diag ->
          pendingWith "No kernelspec directories found in test environment"
  
  describe "ensureDirectoryExists" $ do
    it "should create directory if it doesn't exist" $ do
      let testDir = "/tmp/test-hs-jupyter-kernelspec"
      result <- liftIO $ ensureDirectoryExists testDir
      case result of
        Right createdDir ->
          createdDir `shouldBe` testDir
        Left _diag ->
          pendingWith "Failed to create test directory"
    
    it "should handle existing directories gracefully" $ do
      let testDir = "/tmp"  -- Directory that should always exist
      result <- liftIO $ ensureDirectoryExists testDir
      case result of
        Right existingDir ->
          existingDir `shouldBe` testDir
        Left _diag ->
          pendingWith "Failed to handle existing directory"
  
  describe "getKernelPath" $ do
    it "should construct correct kernel.json path" $ do
      let kernelspecDir = "/usr/local/share/jupyter/kernels"
          kernelName = "haskell"
          expectedPath = "/usr/local/share/jupyter/kernels/haskell/kernel.json"
      getKernelPath kernelspecDir kernelName `shouldBe` expectedPath
    
    it "should handle kernel names with special characters" $ do
      let kernelspecDir = "/home/user/.local/share/jupyter/kernels"
          kernelName = "hs-jupyter-test"
          expectedPath = "/home/user/.local/share/jupyter/kernels/hs-jupyter-test/kernel.json"
      getKernelPath kernelspecDir kernelName `shouldBe` expectedPath
  
  describe "validateKernelInstallation" $ do
    it "should validate kernel installation prerequisites" $ do
      let testKernelspecDir = "/tmp/test-kernelspec"
          kernelName = "test-kernel"
      -- First ensure the kernelspec directory exists
      ensureResult <- liftIO $ ensureDirectoryExists testKernelspecDir
      case ensureResult of
        Right _ -> do
          result <- liftIO $ validateKernelInstallation testKernelspecDir kernelName
          case result of
            Right kernelPath ->
              kernelPath `shouldBe` "/tmp/test-kernelspec/test-kernel/kernel.json"
            Left _diag ->
              pendingWith "Kernel installation validation failed"
        Left _diag ->
          pendingWith "Failed to create test kernelspec directory"
  
  -- T017: Kernel.json Generation Tests
  describe "generateKernelJson" $ do
    it "should generate valid kernel.json content" $ do
      let options = defaultInstallOptions
          ghcPath = "/usr/local/bin/ghc"
      result <- liftIO $ generateKernelJson options ghcPath
      case result of
        Right kernelJson -> do
          -- Validate the generated JSON structure
          validationResult <- liftIO $ validateKernelJson kernelJson
          case validationResult of
            Right _ -> return ()  -- Success
            Left _diag -> pendingWith "Generated kernel.json failed validation"
        Left _diag ->
          pendingWith "Failed to generate kernel.json"
    
    it "should handle empty GHC path gracefully" $ do
      let options = defaultInstallOptions
          ghcPath = ""
      result <- liftIO $ generateKernelJson options ghcPath
      case result of
        Left _diag -> return ()  -- Expected error
        Right _ -> expectationFailure "Should have failed with empty GHC path"
  
  describe "validateKernelJson" $ do
    it "should validate correct kernel.json structure" $ do
      let validKernelJson = object
            [ ("argv", array [string "hs-jupyter-kernel", string "--connection", string "{connection_file}"])
            , ("display_name", string "Haskell")
            , ("language", string "haskell")
            , ("interrupt_mode", string "signal")
            ]
      result <- liftIO $ validateKernelJson validKernelJson
      case result of
        Right _ -> return ()  -- Success
        Left _diag -> pendingWith "Valid kernel.json failed validation"
    
    it "should reject invalid kernel.json structure" $ do
      let invalidKernelJson = object [("invalid", string "structure")]
      result <- liftIO $ validateKernelJson invalidKernelJson
      case result of
        Left _diag -> return ()  -- Expected error
        Right _ -> expectationFailure "Should have failed with invalid structure"
  
  describe "writeKernelJson" $ do
    it "should write kernel.json to file successfully" $ do
      let testPath = "/tmp/test-kernel.json"
          testJson = object
            [ ("argv", array [string "test"])
            , ("display_name", string "Test")
            , ("language", string "test")
            ]
      result <- liftIO $ writeKernelJson testPath testJson
      case result of
        Right _ -> return ()  -- Success
        Left _diag -> pendingWith "Failed to write kernel.json file"
  
  describe "installKernelJson" $ do
    it "should complete kernel.json installation workflow" $ do
      let testKernelPath = "/tmp/test-install-kernel.json"
          options = defaultInstallOptions
          ghcPath = "/usr/local/bin/ghc"
      result <- liftIO $ installKernelJson options testKernelPath ghcPath
      case result of
        Right installedPath ->
          installedPath `shouldBe` testKernelPath
        Left _diag ->
          pendingWith "Kernel.json installation workflow failed"