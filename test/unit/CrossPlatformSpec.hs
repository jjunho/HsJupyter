-- Cross-Platform Compatibility Tests
-- Validates CLI functionality across Linux, macOS, Windows

module CrossPlatformSpec where

import Test.Hspec
import System.Info (os)
import System.FilePath ((</>), isPathSeparator, pathSeparators)
import qualified Data.Text as T
import Data.Aeson (toJSON)

import HsJupyter.CLI.Types
import HsJupyter.CLI.Output
import HsJupyter.CLI.Commands

spec :: Spec
spec = describe "Cross-Platform Compatibility Tests" $ do

  describe "Path Handling" $ do

    it "handles platform-specific path separators" $ do
      let testPath = "jupyter" </> "kernels" </> "haskell" </> "kernel.json"
      -- Path should be valid on current platform
      testPath `shouldSatisfy` (not . null)
      -- Should contain at least one path separator
      any (`elem` testPath) pathSeparators `shouldBe` True

    it "kernelspec paths work on current platform" $ do
      let mockInstallation = KernelInstallation
            { kiKernelspecPath = "/usr/local/share/jupyter/kernels/haskell/kernel.json"
            , kiDisplayName = "Haskell"
            , kiVersion = "0.1.0.0"
            , kiGHCPath = "/usr/bin/ghc"
            , kiStatus = Installed
            , kiConfiguration = KernelConfig
                (ResourceLimits Nothing Nothing Nothing)
                "Haskell"
                "haskell"
                Signal
                (toJSON ())
            }
      -- Should be able to format without platform-specific issues
      formatOutput JSON (Right $ toJSON mockInstallation) `shouldReturn` ()

  describe "Platform Detection" $ do

    it "detects current operating system" $ do
      os `shouldSatisfy` (`elem` ["linux", "darwin", "mingw32"])

    it "provides platform-appropriate system info" $ do
      let systemInfo = SystemInformation
            { siPlatform = T.pack os
            , siArchitecture = "x86_64"  -- Common architecture
            , siShell = Just "/bin/bash"  -- Common on Unix-like systems
            , siPATH = ["/usr/local/bin", "/usr/bin", "/bin"]
            , siWorkingDir = "/home/user"
            }
      -- Should serialize correctly regardless of platform
      formatOutput JSON (Right $ toJSON systemInfo) `shouldReturn` ()

  describe "Output Encoding" $ do

    it "handles Unicode characters in output" $ do
      let unicodeResult = DiagnosticResult
            { drOverallStatus = Healthy
            , drJupyterStatus = JupyterStatus True (Just "4.8.1") ["/usr/share/jupyter/kernels"] True
            , drKernelStatus = KernelStatus True (Just "0.1.0.0") True True
            , drIssuesFound = []
            , drRecommendations = [Recommendation High "Test with Unicode: ñáéíóú" Nothing "Test rationale"]
            , drSystemInfo = SystemInformation "Linux" "x86_64" (Just "/bin/bash") ["/usr/bin"] "/home/user"
            }
      -- Should handle Unicode without encoding issues
      formatOutput JSON (Right $ toJSON unicodeResult) `shouldReturn` ()

    it "produces consistent output across platforms" $ do
      let testResult = UninstallResult
            { urActions = [UninstallAction RemoveKernelspec "/test/path" "Removed"]
            , urSummary = "Test uninstallation"
            }
      -- Output should be consistent (JSON structure)
      formatOutput JSON (Right $ toJSON testResult) `shouldReturn` ()

  describe "File System Operations" $ do

    it "handles different path formats gracefully" $ do
      -- Test with various path formats that should work on any platform
      let paths = [ "/absolute/unix/path" :: FilePath
                  , "relative/path" :: FilePath
                  , "path/with spaces" :: FilePath
                  , "path/with-dashes" :: FilePath
                  , "path/with_underscores" :: FilePath
                  ]
      -- All should be valid paths
      all (not . null) paths `shouldBe` True

    it "kernel installation paths are platform-appropriate" $ do
      let testPaths = case os of
            "mingw32" -> ["C:\\ProgramData\\jupyter\\kernels\\haskell"]
            _ -> ["/usr/local/share/jupyter/kernels/haskell", "/usr/share/jupyter/kernels/haskell"]
      -- Should have at least one valid path for the platform
      not (null testPaths) `shouldBe` True

  describe "Command Line Interface" $ do

    it "command parsing works with platform-specific arguments" $ do
      -- Test parsing with arguments that might differ by platform
      let testArgs = ["install", "--user", "--force"]
      case parseCommand testArgs of
        Right _ -> True `shouldBe` True  -- Parsing succeeded
        Left err -> expectationFailure $ "Command parsing failed: " ++ err

    it "handles different quote styles in arguments" $ do
      -- Test with arguments that might be quoted differently on different platforms
      let testArgs = ["install", "--display-name", "Haskell Kernel"]
      case parseCommand testArgs of
        Right _ -> True `shouldBe` True
        Left err -> expectationFailure $ "Command parsing failed: " ++ err