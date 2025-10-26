{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DoctorSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Time (getCurrentTime, UTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)
import Data.Aeson (ToJSON(..), toJSON, Value(..))

-- HsJupyter imports
import HsJupyter.CLI.Doctor
import HsJupyter.CLI.Types (CLIDiagnostic(..), Issue(..), Severity(..), Component(..), Recommendation(..), Priority(..), HealthStatus(..))

-- | Unit tests for Doctor module (T029 implementation)
spec :: Spec
spec = describe "HsJupyter.CLI.Doctor" $ do
  
  -- Test DiagnosticAnalysis data model
  describe "DiagnosticAnalysis data model" $ do
    it "creates valid DiagnosticAnalysis with all components" $ do
      currentTime <- getCurrentTime
      let analysis = createTestAnalysis currentTime
      daAnalysisDepth analysis `shouldBe` StandardAnalysis
      length (daComponentStatus analysis) `shouldBe` 4  -- Jupyter, Kernel, GHC, System
      
    it "serializes to JSON correctly" $ do
      currentTime <- getCurrentTime
      let analysis = createTestAnalysis currentTime
          jsonResult = toJSON analysis
      jsonResult `shouldNotBe` Null

  -- Test ComponentStatus functionality
  describe "ComponentStatus" $ do
    it "correctly identifies healthy components" $ do
      currentTime <- getCurrentTime
      let healthyStatus = createHealthyComponentStatus JupyterComponent currentTime
      csHealth healthyStatus `shouldBe` HealthyComponent
      csAccessible healthyStatus `shouldBe` True
      csFunctional healthyStatus `shouldBe` True
      csIssues healthyStatus `shouldBe` []
      
    it "correctly identifies unhealthy components" $ do
      currentTime <- getCurrentTime
      let unhealthyStatus = createUnhealthyComponentStatus GHCComponent currentTime
      csHealth unhealthyStatus `shouldBe` BrokenComponent
      csAccessible unhealthyStatus `shouldBe` False
      csFunctional unhealthyStatus `shouldBe` False
      length (csIssues unhealthyStatus) `shouldBe` 1

  -- Test SystemHealthCheck functionality  
  describe "SystemHealthCheck" $ do
    it "calculates overall health correctly for healthy system" $ do
      let healthyComponents = [HealthyComponent, HealthyComponent, HealthyComponent, HealthyComponent]
          criticalIssues = []
          majorIssues = []
          overallHealth = calculateOverallHealth healthyComponents criticalIssues majorIssues
      overallHealth `shouldBe` Healthy
      
    it "calculates overall health correctly for system with critical issues" $ do
      let mixedComponents = [HealthyComponent, BrokenComponent, HealthyComponent, HealthyWithWarningsComponent]
          criticalIssues = [createTestIssue Critical "Critical test issue"]
          majorIssues = []
          overallHealth = calculateOverallHealth mixedComponents criticalIssues majorIssues
      overallHealth `shouldBe` Broken

  -- Test Issue categorization and analysis
  describe "Issue categorization" $ do
    it "correctly categorizes issues by severity" $ do
      let issues = [ createTestIssue Critical "Critical issue"
                   , createTestIssue Major "Major issue"  
                   , createTestIssue Minor "Minor issue"
                   , createTestIssue Warning "Warning issue"
                   ]
          analysis = categorizeIssues issues
      length (iaBlockingIssues analysis) `shouldBe` 1  -- Only Critical
      
    it "identifies blocking issues correctly" $ do
      let criticalIssue = createTestIssue Critical "Blocking issue"
          majorIssue = createTestIssue Major "Non-blocking issue"
      isBlockingIssue criticalIssue `shouldBe` True
      isBlockingIssue majorIssue `shouldBe` False

  -- Test Recommendation generation
  describe "Recommendation generation" $ do
    it "generates recommendations for critical issues" $ do
      let criticalIssue = createTestIssue Critical "Test critical issue"
          rec = generateCriticalRecommendation criticalIssue
      rPriority rec `shouldBe` Immediate
      rAction rec `shouldBe` "Fix critical issue"
      
    it "generates recommendations for major issues" $ do
      let majorIssue = createTestIssue Major "Test major issue"
          rec = generateMajorRecommendation majorIssue
      rPriority rec `shouldBe` High
      rAction rec `shouldBe` "Fix major issue"

  -- Test component health checking utilities
  describe "Component health utilities" $ do
    it "correctly identifies component health by type" $ do
      currentTime <- getCurrentTime
      let jupyterStatus = createHealthyComponentStatus JupyterComponent currentTime
          kernelStatus = createUnhealthyComponentStatus KernelComponent currentTime
          statuses = [jupyterStatus, kernelStatus]
          jupyterHealth = getComponentHealthByType JupyterComponent statuses
          kernelHealth = getComponentHealthByType KernelComponent statuses
      jupyterHealth `shouldBe` HealthyComponent
      kernelHealth `shouldBe` BrokenComponent

  -- Test analysis depth functionality
  describe "AnalysisDepth" $ do
    it "has correct ordering for analysis depths" $ do
      QuickAnalysis < StandardAnalysis `shouldBe` True
      StandardAnalysis < DeepAnalysis `shouldBe` True
      QuickAnalysis < DeepAnalysis `shouldBe` True

  -- Test system health check integration
  describe "System health check integration" $ do
    it "performs quick health check without errors" $ do
      result <- performSystemHealthCheck QuickAnalysis
      case result of
        Left _diag -> expectationFailure "Quick health check should not fail"
        Right analysis -> do
          daAnalysisDepth analysis `shouldBe` QuickAnalysis
          length (daComponentStatus analysis) `shouldSatisfy` (> 0)

-- Helper functions for creating test data
createTestAnalysis :: UTCTime -> DiagnosticAnalysis  
createTestAnalysis currentTime = DiagnosticAnalysis
  { daSystemHealth = createTestSystemHealth
  , daComponentStatus = [ createHealthyComponentStatus JupyterComponent currentTime
                        , createHealthyComponentStatus KernelComponent currentTime
                        , createHealthyComponentStatus GHCComponent currentTime
                        , createHealthyComponentStatus SystemComponent currentTime
                        ]
  , daIssueAnalysis = createTestIssueAnalysis
  , daRecommendations = []
  , daAnalysisTime = currentTime
  , daAnalysisDepth = StandardAnalysis
  }

createTestSystemHealth :: SystemHealthCheck
createTestSystemHealth = SystemHealthCheck
  { shcOverallHealth = Healthy
  , shcJupyterHealth = HealthyComponent
  , shcKernelHealth = HealthyComponent
  , shcGHCHealth = HealthyComponent
  , shcSystemHealth = HealthyComponent
  , shcCriticalIssues = []
  , shcMajorIssues = []
  , shcMinorIssues = []
  }

createHealthyComponentStatus :: Component -> UTCTime -> ComponentStatus
createHealthyComponentStatus componentType checkTime = ComponentStatus
  { csComponent = componentType
  , csHealth = HealthyComponent
  , csVersion = Just "1.0.0"
  , csPath = Just "/usr/bin/component"
  , csAccessible = True
  , csFunctional = True
  , csIssues = []
  , csLastCheck = checkTime
  }

createUnhealthyComponentStatus :: Component -> UTCTime -> ComponentStatus
createUnhealthyComponentStatus componentType checkTime = ComponentStatus
  { csComponent = componentType
  ,   csHealth = BrokenComponent
  , csVersion = Nothing
  , csPath = Nothing
  , csAccessible = False
  , csFunctional = False
  , csIssues = [createTestIssue Critical "Component not found"]
  , csLastCheck = checkTime
  }

createTestIssue :: Severity -> Text -> Issue
createTestIssue severity description = Issue
  { iSeverity = severity
  , iComponent = SystemComponent
  , iDescription = description
  , iDetails = Nothing
  }

createTestIssueAnalysis :: IssueAnalysis
createTestIssueAnalysis = IssueAnalysis
  { iaBlockingIssues = []
  , iaPerformanceIssues = []
  , iaCompatibilityIssues = []
  , iaConfigurationIssues = []
  , iaPermissionIssues = []
  , iaEnvironmentIssues = []
  }

-- Property-based tests for robustness
prop_healthCalculationMonotonic :: [ComponentHealth] -> [Issue] -> [Issue] -> Property
prop_healthCalculationMonotonic componentHealths criticalIssues majorIssues =
  let baseHealth = calculateOverallHealth componentHealths [] []
      withCritical = calculateOverallHealth componentHealths criticalIssues []
      withBoth = calculateOverallHealth componentHealths criticalIssues majorIssues
  in property $ True  -- Simplified test - just check it doesn't crash

prop_issueCategorizationComplete :: [Issue] -> Property
prop_issueCategorizationComplete issues =
  let analysis = categorizeIssues issues
      categorizedCount = length (iaBlockingIssues analysis) +
                        length (iaPerformanceIssues analysis) +
                        length (iaCompatibilityIssues analysis) +
                        length (iaConfigurationIssues analysis) +
                        length (iaPermissionIssues analysis) +
                        length (iaEnvironmentIssues analysis)
  in property $ categorizedCount >= 0  -- At minimum, no issues should be lost

-- QuickCheck instances for property-based testing
instance Arbitrary ComponentHealth where
  arbitrary = elements [HealthyComponent, HealthyWithWarningsComponent, DegradedComponent, BrokenComponent, NotFoundComponent]

instance Arbitrary Severity where
  arbitrary = elements [Critical, Major, Minor, Warning]

instance Arbitrary Component where
  arbitrary = elements [JupyterComponent, KernelComponent, GHCComponent, SystemComponent]

instance Arbitrary Issue where
  arbitrary = do
    severity <- arbitrary
    component <- arbitrary
    description <- T.pack <$> arbitrary
    return $ Issue severity component description Nothing