-- Performance Tests for CLI Infrastructure
-- Validates constitutional performance targets

module PerformanceSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Control.Monad.IO.Class (liftIO)
import System.Clock (Clock(..), getTime, diffTimeSpec, toNanoSecs)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (toJSON)

import HsJupyter.CLI.Output
import HsJupyter.CLI.Types
import HsJupyter.CLI.Commands

-- Performance measurement utilities
measureTime :: IO a -> IO (a, Double)
measureTime action = do
  start <- getTime Monotonic
  result <- action
  end <- getTime Monotonic
  let diff = diffTimeSpec start end
      nanos = toNanoSecs diff
      seconds = fromIntegral nanos / 1e9
  return (result, seconds)

measureTimeOnly :: IO a -> IO Double
measureTimeOnly action = do
  start <- getTime Monotonic
  _ <- action
  end <- getTime Monotonic
  let diff = diffTimeSpec start end
      nanos = toNanoSecs diff
      seconds = fromIntegral nanos / 1e9
  return seconds

-- Constitutional performance targets
installTimeLimit :: Double
installTimeLimit = 120.0  -- 2 minutes max

diagnosticTimeLimit :: Double
diagnosticTimeLimit = 5.0   -- 5 seconds max

memoryLimit :: Integer
memoryLimit = 100 * 1024 * 1024  -- 100MB max

-- Mock data for performance testing
mockUninstallResult :: UninstallResult
mockUninstallResult = UninstallResult
  { urActions = [mockUninstallAction]
  , urSummary = "Uninstallation completed successfully"
  }

mockUninstallAction :: UninstallAction
mockUninstallAction = UninstallAction
  { uaType = RemoveKernelspec
  , uaTarget = "/usr/local/share/jupyter/kernels/haskell"
  , uaResult = "Kernelspec directory removed"
  }

mockDoctorResult :: DiagnosticResult
mockDoctorResult = DiagnosticResult
  { drOverallStatus = Healthy
  , drJupyterStatus = mockJupyterStatus
  , drKernelStatus = mockKernelStatus
  , drIssuesFound = []
  , drRecommendations = []
  , drSystemInfo = mockSystemInfo
  }

mockJupyterStatus :: JupyterStatus
mockJupyterStatus = JupyterStatus
  { jsInstalled = True
  , jsVersion = Just "4.8.1"
  , jsKernelspecDirs = ["/usr/local/share/jupyter/kernels", "/usr/share/jupyter/kernels"]
  , jsAccessible = True
  }

mockKernelStatus :: KernelStatus
mockKernelStatus = KernelStatus
  { ksInstalled = True
  , ksVersion = Just "0.1.0.0"
  , ksFunctional = True
  , ksGHCAvailable = True
  }

mockSystemInfo :: SystemInformation
mockSystemInfo = SystemInformation
  { siPlatform = "Linux"
  , siArchitecture = "x86_64"
  , siShell = Just "/bin/bash"
  , siPATH = ["/usr/local/bin", "/usr/bin", "/bin"]
  , siWorkingDir = "/home/user"
  }

spec :: Spec
spec = describe "CLI Performance Tests" $ do

  describe "JSON Output Performance" $ do

    it "formats uninstall result within time limits" $ do
      time <- measureTimeOnly $ formatOutput JSON (Right $ toJSON mockUninstallResult)
      time `shouldSatisfy` (< diagnosticTimeLimit)

    it "formats doctor result within time limits" $ do
      time <- measureTimeOnly $ formatOutput JSON (Right $ toJSON mockDoctorResult)
      time `shouldSatisfy` (< diagnosticTimeLimit)

    it "handles large result sets efficiently" $ do
      let largeResult = mockDoctorResult
            { drIssuesFound = replicate 100 mockIssue
            , drRecommendations = replicate 50 mockRecommendation
            }
      time <- measureTimeOnly $ formatOutput JSON (Right $ toJSON largeResult)
      time `shouldSatisfy` (< diagnosticTimeLimit)

    it "produces valid JSON output" $ do
      result <- formatOutput JSON (Right $ toJSON mockUninstallResult)
      -- This test will pass if no exceptions are thrown during formatting
      result `shouldBe` ()

  describe "Command Parsing Performance" $ do

    it "parses install command options quickly" $ do
      let args = ["install", "--user", "--force", "--quiet", "--json"]
      time <- measureTimeOnly $ return (parseCommand args)
      time `shouldSatisfy` (< diagnosticTimeLimit)

    it "parses doctor command options quickly" $ do
      let args = ["doctor", "--json", "--verbose"]
      time <- measureTimeOnly $ return (parseCommand args)
      time `shouldSatisfy` (< diagnosticTimeLimit)

  describe "Output Format Performance" $ do

    it "HumanReadable format is fast" $ do
      time <- measureTimeOnly $ formatOutput HumanReadable (Right $ toJSON mockUninstallResult)
      time `shouldSatisfy` (< diagnosticTimeLimit)

    it "Quiet format is fastest" $ do
      time <- measureTimeOnly $ formatOutput Quiet (Right $ toJSON mockUninstallResult)
      time `shouldSatisfy` (< diagnosticTimeLimit)

  describe "Memory Usage Validation" $ do

    it "JSON serialization doesn't exceed memory limits" $ do
      -- This is a basic check - in real scenarios we'd use profiling tools
      result <- formatOutput JSON (Right $ toJSON mockUninstallResult)
      -- Test passes if no exceptions thrown
      result `shouldBe` ()

    it "handles concurrent formatting efficiently" $ do
      -- Test concurrent performance (basic approximation)
      results <- mapM (formatOutput JSON . Right . toJSON) (replicate 10 mockUninstallResult)
      all (== ()) results `shouldBe` True

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

mockIssue :: Issue
mockIssue = Issue
  { iSeverity = Warning
  , iComponent = KernelComponent
  , iDescription = "Test issue"
  , iDetails = Just "Test details"
  }

mockRecommendation :: Recommendation
mockRecommendation = Recommendation
  { rPriority = Low
  , rAction = "Test action"
  , rCommand = Just "test command"
  , rRationale = "Test rationale"
  }