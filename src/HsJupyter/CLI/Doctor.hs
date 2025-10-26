{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | CLI Doctor module - handles diagnostic commands and system health analysis
module HsJupyter.CLI.Doctor
  ( executeDiagnostics
  , analyzeSystemHealth
  , generateDiagnosticReport
  , identifyIssues
  , generateRecommendations
  -- T025: DiagnosticResult data model and analysis logic
  , DiagnosticAnalysis(..)
  , SystemHealthCheck(..)
  , ComponentStatus(..)
  , ComponentHealth(..)
  , IssueAnalysis(..)
  , AnalysisDepth(..)
  , Recommendation(..)
  , Priority(..)
  , performSystemHealthCheck
  , categorizeIssues
  , calculateOverallHealth
  , getComponentHealthByType
  , generateCriticalRecommendation
  , generateMajorRecommendation
  , generateBlockingRecommendation
  , isBlockingIssue
  , analyzeDiagnosticResult
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (filterM, when)
import Control.Exception (try, IOException, SomeException, catch)
import System.Directory 
  ( doesDirectoryExist
  , doesFileExist
  , getPermissions
  , writable
  , readable
  )
import System.FilePath ((</>))
import System.Environment (lookupEnv)
import Data.Aeson (Value(..), ToJSON(..), (.=), object)
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Time.Clock (getCurrentTime, UTCTime)

-- Constitutional integration
import HsJupyter.Runtime.ErrorHandling (withErrorContext)
import HsJupyter.Runtime.ResourceGuard 
  ( withResourceGuard
  , defaultResourceLimits
  , ResourceLimits(..)
  )
import HsJupyter.Runtime.Telemetry (RuntimeMetric(..), emitMetric)

-- CLI Types integration
import HsJupyter.CLI.Types 
  ( CLIDiagnostic(..)
  , DiagnosticResult(..)
  , JupyterEnvironment(..)
  , PythonEnvironment(..)
  , JupyterStatus(..)
  , KernelStatus(..)
  , HealthStatus(..)
  , Issue(..)
  , Severity(..)
  , Component(..)
  , Recommendation(..)
  , Priority(..)
  , SystemInformation(..)
  )
import HsJupyter.CLI.Commands (GlobalOptions(..))
import HsJupyter.CLI.Install (detectJupyterEnvironment, logCLIOperation)
import qualified HsJupyter.CLI.Utilities as Utilities

-- ===========================================================================
-- T025: DiagnosticResult Data Model and Analysis Logic
-- ===========================================================================

-- | Enhanced diagnostic analysis context for T025 implementation
data DiagnosticAnalysis = DiagnosticAnalysis
  { daSystemHealth     :: SystemHealthCheck   -- Overall system health assessment
  , daComponentStatus  :: [ComponentStatus]   -- Individual component statuses
  , daIssueAnalysis    :: IssueAnalysis       -- Categorized issue analysis
  , daRecommendations  :: [Recommendation]    -- Prioritized recommendations
  , daAnalysisTime     :: UTCTime             -- Timestamp of analysis
  , daAnalysisDepth    :: AnalysisDepth       -- Depth of analysis performed
  } deriving (Show, Eq)

-- | System-wide health check results
data SystemHealthCheck = SystemHealthCheck
  { shcOverallHealth      :: HealthStatus     -- Overall system health
  , shcJupyterHealth      :: ComponentHealth  -- Jupyter component health
  , shcKernelHealth       :: ComponentHealth  -- Kernel component health
  , shcGHCHealth          :: ComponentHealth  -- GHC component health
  , shcSystemHealth       :: ComponentHealth  -- System environment health
  , shcCriticalIssues     :: [Issue]          -- Critical blocking issues
  , shcMajorIssues        :: [Issue]          -- Major functionality issues
  , shcMinorIssues        :: [Issue]          -- Minor/cosmetic issues
  } deriving (Show, Eq)

-- | Individual component status and health
data ComponentStatus = ComponentStatus
  { csComponent       :: Component            -- Component identifier
  , csHealth          :: ComponentHealth      -- Health assessment
  , csVersion         :: Maybe Text           -- Component version if available
  , csPath            :: Maybe FilePath       -- Component path if applicable
  , csAccessible      :: Bool                 -- Component accessibility
  , csFunctional      :: Bool                 -- Component functionality
  , csIssues          :: [Issue]              -- Component-specific issues
  , csLastCheck       :: UTCTime              -- Last health check time
  } deriving (Show, Eq)

-- | Component health assessment levels
data ComponentHealth
  = HealthyComponent                          -- Fully functional
  | HealthyWithWarningsComponent              -- Functional but has warnings
  | DegradedComponent                         -- Partially functional
  | BrokenComponent                           -- Non-functional
  | NotFoundComponent                         -- Component not present
  deriving (Show, Eq, Ord)

-- | Issue analysis categorization
data IssueAnalysis = IssueAnalysis
  { iaBlockingIssues      :: [Issue]          -- Issues preventing functionality
  , iaPerformanceIssues   :: [Issue]          -- Issues affecting performance
  , iaCompatibilityIssues :: [Issue]          -- Version/compatibility issues
  , iaConfigurationIssues :: [Issue]          -- Configuration problems
  , iaPermissionIssues    :: [Issue]          -- Permission/access issues
  , iaEnvironmentIssues   :: [Issue]          -- Environment setup issues
  } deriving (Show, Eq)

-- | Analysis depth levels for diagnostic operations
data AnalysisDepth
  = QuickAnalysis         -- Fast basic checks (<1 second)
  | StandardAnalysis      -- Comprehensive checks (<5 seconds)
  | DeepAnalysis          -- Thorough analysis including functionality tests
  deriving (Show, Eq, Ord)

instance ToJSON DiagnosticAnalysis where
  toJSON da = object
    [ "system_health" .= daSystemHealth da
    , "component_status" .= daComponentStatus da
    , "issue_analysis" .= daIssueAnalysis da
    , "recommendations" .= daRecommendations da
    , "analysis_time" .= daAnalysisTime da
    , "analysis_depth" .= show (daAnalysisDepth da)
    ]

instance ToJSON SystemHealthCheck where
  toJSON shc = object
    [ "overall_health" .= show (shcOverallHealth shc)
    , "jupyter_health" .= show (shcJupyterHealth shc)
    , "kernel_health" .= show (shcKernelHealth shc)
    , "ghc_health" .= show (shcGHCHealth shc)
    , "system_health" .= show (shcSystemHealth shc)
    , "critical_issues" .= shcCriticalIssues shc
    , "major_issues" .= shcMajorIssues shc
    , "minor_issues" .= shcMinorIssues shc
    ]

instance ToJSON ComponentStatus where
  toJSON cs = object
    [ "component" .= show (csComponent cs)
    , "health" .= show (csHealth cs)
    , "version" .= csVersion cs
    , "path" .= csPath cs
    , "accessible" .= csAccessible cs
    , "functional" .= csFunctional cs
    , "issues" .= csIssues cs
    , "last_check" .= csLastCheck cs
    ]

instance ToJSON IssueAnalysis where
  toJSON ia = object
    [ "blocking_issues" .= iaBlockingIssues ia
    , "performance_issues" .= iaPerformanceIssues ia
    , "compatibility_issues" .= iaCompatibilityIssues ia
    , "configuration_issues" .= iaConfigurationIssues ia
    , "permission_issues" .= iaPermissionIssues ia
    , "environment_issues" .= iaEnvironmentIssues ia
    ]

-- ===========================================================================
-- T025: Core Diagnostic Functions
-- ===========================================================================

-- | Execute comprehensive diagnostic analysis (T025 main entry point)
executeDiagnostics :: GlobalOptions -> IO (Either CLIDiagnostic DiagnosticResult)
executeDiagnostics globalOpts = withErrorContext "system-diagnostics" $ do
  logCLIOperation "diagnostics" "Starting comprehensive system diagnostics" 
    [ ("analysis_depth", A.String "standard")
    , ("json_output", A.Bool $ goJSON globalOpts)
    ]
  
  -- Constitutional resource limits for diagnostic operations (<5s per spec)
  let diagnosticsLimits = defaultResourceLimits
        { rcMaxCpuSeconds = 5.0      -- 5 second timeout per constitutional spec
        , rcMaxMemoryMB = 50         -- 50MB memory limit for diagnostic operations
        , rcMaxOutputBytes = 1048576 -- 1MB output limit for diagnostic reports
        }
  
  result <- withResourceGuard diagnosticsLimits $ \guard -> do
    analysisResult <- performSystemHealthCheck StandardAnalysis
    case analysisResult of
      Left diag -> return $ Left diag
      Right analysis -> do
        -- Generate comprehensive diagnostic result
        diagnosticResult <- analyzeDiagnosticResult analysis
        case diagnosticResult of
          Left diag -> return $ Left diag
          Right result -> return $ Right result
  
  return result

-- | Perform comprehensive system health check (T025 implementation)
performSystemHealthCheck :: AnalysisDepth -> IO (Either CLIDiagnostic DiagnosticAnalysis)
performSystemHealthCheck depth = do
  analysisTime <- getCurrentTime
  
  logCLIOperation "health_check" ("Performing system health check at depth: " ++ show depth) 
    [("depth", A.String $ T.pack $ show depth)]
  
  -- Step 1: Check individual components
  componentStatusResults <- checkAllComponents depth
  case componentStatusResults of
    Left diag -> return $ Left diag
    Right componentStatuses -> do
      -- Step 2: Analyze system-wide health
      systemHealthResult <- analyzeSystemWideHealth componentStatuses
      case systemHealthResult of
        Left diag -> return $ Left diag
        Right systemHealth -> do
          -- Step 3: Categorize and analyze issues
          let issueAnalysis = categorizeIssues (concatMap csIssues componentStatuses)
          
          -- Step 4: Generate prioritized recommendations  
          recommendations <- generateRecommendations systemHealth issueAnalysis
          
          let analysis = DiagnosticAnalysis
                { daSystemHealth = systemHealth
                , daComponentStatus = componentStatuses
                , daIssueAnalysis = issueAnalysis
                , daRecommendations = recommendations
                , daAnalysisTime = analysisTime
                , daAnalysisDepth = depth
                }
          
          return $ Right analysis

-- | Check all system components individually
checkAllComponents :: AnalysisDepth -> IO (Either CLIDiagnostic [ComponentStatus])
checkAllComponents depth = do
  currentTime <- getCurrentTime
  
  -- Check Jupyter component
  jupyterResult <- checkJupyterComponent depth currentTime
  case jupyterResult of
    Left diag -> return $ Left diag
    Right jupyterStatus -> do
      -- Check Kernel component
      kernelResult <- checkKernelComponent depth currentTime
      case kernelResult of
        Left diag -> return $ Left diag
        Right kernelStatus -> do
          -- Check GHC component
          ghcResult <- checkGHCComponent depth currentTime
          case ghcResult of
            Left diag -> return $ Left diag
            Right ghcStatus -> do
              -- Check System component
              systemResult <- checkSystemComponent depth currentTime
              case systemResult of
                Left diag -> return $ Left diag
                Right systemStatus -> 
                  return $ Right [jupyterStatus, kernelStatus, ghcStatus, systemStatus]

-- | Analyze system-wide health based on component statuses
analyzeSystemWideHealth :: [ComponentStatus] -> IO (Either CLIDiagnostic SystemHealthCheck)
analyzeSystemWideHealth componentStatuses = do
  let allIssues = concatMap csIssues componentStatuses
      criticalIssues = filter (\i -> iSeverity i == Critical) allIssues
      majorIssues = filter (\i -> iSeverity i == Major) allIssues
      minorIssues = filter (\i -> iSeverity i `elem` [Minor, Warning]) allIssues
      
      -- Determine component health levels
      jupyterHealth = getComponentHealthByType JupyterComponent componentStatuses
      kernelHealth = getComponentHealthByType KernelComponent componentStatuses
      ghcHealth = getComponentHealthByType GHCComponent componentStatuses
      systemHealth = getComponentHealthByType SystemComponent componentStatuses
      
      -- Calculate overall health based on component health and issues
      overallHealth = calculateOverallHealth 
        [jupyterHealth, kernelHealth, ghcHealth, systemHealth] 
        criticalIssues majorIssues
  
  let systemHealthCheck = SystemHealthCheck
        { shcOverallHealth = overallHealth
        , shcJupyterHealth = jupyterHealth
        , shcKernelHealth = kernelHealth
        , shcGHCHealth = ghcHealth
        , shcSystemHealth = systemHealth
        , shcCriticalIssues = criticalIssues
        , shcMajorIssues = majorIssues
        , shcMinorIssues = minorIssues
        }
  
  return $ Right systemHealthCheck

-- | Convert diagnostic analysis to constitutional DiagnosticResult format
analyzeDiagnosticResult :: DiagnosticAnalysis -> IO (Either CLIDiagnostic DiagnosticResult)
analyzeDiagnosticResult analysis = do
  -- Convert analysis to constitutional DiagnosticResult format
  let systemInfo = extractSystemInformation analysis
      
      jupyterStatus = JupyterStatus
        { jsInstalled = isComponentHealthy JupyterComponent (daComponentStatus analysis)
        , jsVersion = getComponentVersion JupyterComponent (daComponentStatus analysis)
        , jsKernelspecDirs = []  -- Will be populated by component analysis
        , jsAccessible = isComponentAccessible JupyterComponent (daComponentStatus analysis)
        }
        
      kernelStatus = KernelStatus
        { ksInstalled = isComponentHealthy KernelComponent (daComponentStatus analysis)
        , ksVersion = getComponentVersion KernelComponent (daComponentStatus analysis)
        , ksFunctional = isComponentFunctional KernelComponent (daComponentStatus analysis)
        , ksGHCAvailable = isComponentHealthy GHCComponent (daComponentStatus analysis)
        }
        
      diagnosticResult = DiagnosticResult
        { drOverallStatus = shcOverallHealth (daSystemHealth analysis)
        , drJupyterStatus = jupyterStatus
        , drKernelStatus = kernelStatus
        , drIssuesFound = getAllIssuesFromAnalysis analysis
        , drRecommendations = daRecommendations analysis
        , drSystemInfo = systemInfo
        }
  
  return $ Right diagnosticResult

-- ===========================================================================
-- T025: Component Health Check Functions
-- ===========================================================================

-- | Check Jupyter component health and status
checkJupyterComponent :: AnalysisDepth -> UTCTime -> IO (Either CLIDiagnostic ComponentStatus)
checkJupyterComponent depth checkTime = do
  -- Use existing Jupyter detection logic
  jupyterEnvResult <- detectJupyterEnvironment
  case jupyterEnvResult of
    Left diag -> do
      -- Jupyter not found or inaccessible
      let status = ComponentStatus
            { csComponent = JupyterComponent
            , csHealth = NotFoundComponent
            , csVersion = Nothing
            , csPath = Nothing
            , csAccessible = False
            , csFunctional = False
            , csIssues = [Issue Critical JupyterComponent "Jupyter environment not detected" (Just $ T.pack $ show diag)]
            , csLastCheck = checkTime
            }
      return $ Right status
    Right jupyterEnv -> do
      -- Analyze Jupyter environment health
      let jupyterIssues = validateJupyterEnvironment jupyterEnv
          health = determineJupyterHealth jupyterEnv jupyterIssues
          version = extractJupyterVersion jupyterEnv
          
      let status = ComponentStatus
            { csComponent = JupyterComponent
            , csHealth = health
            , csVersion = version
            , csPath = Nothing  -- Jupyter is a distributed installation
            , csAccessible = not (null $ jeKernelspecDirs jupyterEnv)
            , csFunctional = health `elem` [HealthyComponent, HealthyWithWarningsComponent]
            , csIssues = jupyterIssues
            , csLastCheck = checkTime
            }
      return $ Right status

-- | Check HsJupyter kernel component health
checkKernelComponent :: AnalysisDepth -> UTCTime -> IO (Either CLIDiagnostic ComponentStatus)
checkKernelComponent depth checkTime = do
  -- TODO: Implement kernel detection logic
  -- For now, return placeholder status
  let status = ComponentStatus
        { csComponent = KernelComponent
        , csHealth = NotFoundComponent  -- Will be updated with actual detection
        , csVersion = Nothing
        , csPath = Nothing
        , csAccessible = False
        , csFunctional = False
        , csIssues = []
        , csLastCheck = checkTime
        }
  return $ Right status

-- | Check GHC component health
checkGHCComponent :: AnalysisDepth -> UTCTime -> IO (Either CLIDiagnostic ComponentStatus)
checkGHCComponent depth checkTime = do
  -- Check for GHC installation and accessibility
  ghcPathResult <- findGHCExecutable
  case ghcPathResult of
    Nothing -> do
      let status = ComponentStatus
            { csComponent = GHCComponent
            , csHealth = NotFoundComponent
            , csVersion = Nothing
            , csPath = Nothing
            , csAccessible = False
            , csFunctional = False
            , csIssues = [Issue Critical GHCComponent "GHC executable not found in PATH" Nothing]
            , csLastCheck = checkTime
            }
      return $ Right status
    Just ghcPath -> do
      -- Test GHC functionality if deep analysis requested
      ghcResult <- case depth of
        QuickAnalysis -> return $ Right (Just "unknown")
        StandardAnalysis -> testGHCVersion ghcPath
        DeepAnalysis -> testGHCFunctionality ghcPath
      
      case ghcResult of
        Left errMsg -> do
          let status = ComponentStatus
                { csComponent = GHCComponent
                , csHealth = BrokenComponent
                , csVersion = Nothing
                , csPath = Just ghcPath
                , csAccessible = True
                , csFunctional = False
                , csIssues = [Issue Major GHCComponent ("GHC not functional: " <> errMsg) Nothing]
                , csLastCheck = checkTime
                }
          return $ Right status
        Right version -> do
          let status = ComponentStatus
                { csComponent = GHCComponent
                , csHealth = HealthyComponent
                , csVersion = version
                , csPath = Just ghcPath
                , csAccessible = True
                , csFunctional = True
                , csIssues = []
                , csLastCheck = checkTime
                }
          return $ Right status

-- | Check system environment component
checkSystemComponent :: AnalysisDepth -> UTCTime -> IO (Either CLIDiagnostic ComponentStatus)
checkSystemComponent depth checkTime = do
  -- Gather system information
  systemInfo <- gatherSystemInformation
  let systemIssues = validateSystemEnvironment systemInfo
      health = if null systemIssues then HealthyComponent else HealthyWithWarningsComponent
      
  let status = ComponentStatus
        { csComponent = SystemComponent
        , csHealth = health
        , csVersion = Nothing
        , csPath = Nothing
        , csAccessible = True
        , csFunctional = True
        , csIssues = systemIssues
        , csLastCheck = checkTime
        }
  return $ Right status

-- ===========================================================================
-- T025: Helper and Analysis Functions
-- ===========================================================================

-- | Categorize issues by type for structured analysis
categorizeIssues :: [Issue] -> IssueAnalysis
categorizeIssues issues = IssueAnalysis
  { iaBlockingIssues = filter isBlockingIssue issues
  , iaPerformanceIssues = filter isPerformanceIssue issues
  , iaCompatibilityIssues = filter isCompatibilityIssue issues
  , iaConfigurationIssues = filter isConfigurationIssue issues
  , iaPermissionIssues = filter isPermissionIssue issues
  , iaEnvironmentIssues = filter isEnvironmentIssue issues
  }

-- | Generate prioritized recommendations based on system health and issues
generateRecommendations :: SystemHealthCheck -> IssueAnalysis -> IO [Recommendation]
generateRecommendations systemHealth issueAnalysis = do
  let criticalRecs = map generateCriticalRecommendation (shcCriticalIssues systemHealth)
      majorRecs = map generateMajorRecommendation (shcMajorIssues systemHealth) 
      blockingRecs = map generateBlockingRecommendation (iaBlockingIssues issueAnalysis)
      
      allRecommendations = criticalRecs ++ majorRecs ++ blockingRecs
      sortedRecommendations = sortRecommendationsByPriority allRecommendations
  
  return sortedRecommendations

-- Helper functions for component analysis
getComponentHealthByType :: Component -> [ComponentStatus] -> ComponentHealth
getComponentHealthByType targetComponent statuses =
  case filter (\cs -> csComponent cs == targetComponent) statuses of
    [] -> NotFoundComponent
    (cs:_) -> csHealth cs

calculateOverallHealth :: [ComponentHealth] -> [Issue] -> [Issue] -> HealthStatus
calculateOverallHealth componentHealths criticalIssues majorIssues
  | not (null criticalIssues) = Broken
  | any (== BrokenComponent) componentHealths = Broken  
  | not (null majorIssues) = Degraded
  | any (== DegradedComponent) componentHealths = Degraded
  | any (== HealthyWithWarningsComponent) componentHealths = HealthyWithWarnings
  | otherwise = Healthy

-- Placeholder helper functions (to be implemented in subsequent tasks)
validateJupyterEnvironment :: JupyterEnvironment -> [Issue]
validateJupyterEnvironment = const []  -- Placeholder

determineJupyterHealth :: JupyterEnvironment -> [Issue] -> ComponentHealth
determineJupyterHealth _ [] = HealthyComponent
determineJupyterHealth _ _ = HealthyWithWarningsComponent

extractJupyterVersion :: JupyterEnvironment -> Maybe Text
extractJupyterVersion = const Nothing  -- Placeholder

findGHCExecutable :: IO (Maybe FilePath)
findGHCExecutable = return Nothing  -- Placeholder

testGHCVersion :: FilePath -> IO (Either Text (Maybe Text))
testGHCVersion = const $ return $ Right $ Just "9.12.2"  -- Placeholder

testGHCFunctionality :: FilePath -> IO (Either Text (Maybe Text))
testGHCFunctionality = const $ return $ Right $ Just "9.12.2"  -- Placeholder

gatherSystemInformation :: IO SystemInformation
gatherSystemInformation = do
  return SystemInformation  -- Placeholder implementation
    { siPlatform = "linux"
    , siArchitecture = "x86_64"
    , siShell = Nothing
    , siPATH = []
    , siWorkingDir = "."
    }

validateSystemEnvironment :: SystemInformation -> [Issue]
validateSystemEnvironment = const []  -- Placeholder

-- Issue classification helpers
isBlockingIssue, isPerformanceIssue, isCompatibilityIssue :: Issue -> Bool
isConfigurationIssue, isPermissionIssue, isEnvironmentIssue :: Issue -> Bool
isBlockingIssue i = iSeverity i == Critical
isPerformanceIssue _ = False  -- Placeholder
isCompatibilityIssue _ = False  -- Placeholder  
isConfigurationIssue _ = False  -- Placeholder
isPermissionIssue _ = False  -- Placeholder
isEnvironmentIssue _ = False  -- Placeholder

-- Recommendation generation helpers
generateCriticalRecommendation, generateMajorRecommendation :: Issue -> Recommendation
generateBlockingRecommendation :: Issue -> Recommendation
generateCriticalRecommendation issue = Recommendation Immediate "Fix critical issue" Nothing (iDescription issue)
generateMajorRecommendation issue = Recommendation High "Fix major issue" Nothing (iDescription issue)
generateBlockingRecommendation issue = Recommendation Immediate "Fix blocking issue" Nothing (iDescription issue)

sortRecommendationsByPriority :: [Recommendation] -> [Recommendation]
sortRecommendationsByPriority = id  -- Placeholder

-- Diagnostic result extraction helpers
extractSystemInformation :: DiagnosticAnalysis -> SystemInformation
extractSystemInformation _ = SystemInformation "linux" "x86_64" Nothing [] "."  -- Placeholder

isComponentHealthy, isComponentAccessible, isComponentFunctional :: Component -> [ComponentStatus] -> Bool
isComponentHealthy comp statuses = getComponentHealthByType comp statuses == HealthyComponent
isComponentAccessible comp statuses = maybe False csAccessible $ findComponentStatus comp statuses
isComponentFunctional comp statuses = maybe False csFunctional $ findComponentStatus comp statuses

getComponentVersion :: Component -> [ComponentStatus] -> Maybe Text
getComponentVersion comp statuses = findComponentStatus comp statuses >>= csVersion

findComponentStatus :: Component -> [ComponentStatus] -> Maybe ComponentStatus
findComponentStatus comp statuses = case filter (\cs -> csComponent cs == comp) statuses of
  [] -> Nothing
  (cs:_) -> Just cs

getAllIssuesFromAnalysis :: DiagnosticAnalysis -> [Issue]
getAllIssuesFromAnalysis analysis = concatMap csIssues (daComponentStatus analysis)

-- ===========================================================================
-- Public API Functions (for integration with CLI commands)
-- ===========================================================================

-- | Analyze system health (public API for CLI integration)
analyzeSystemHealth :: GlobalOptions -> IO (Either CLIDiagnostic DiagnosticResult)
analyzeSystemHealth = executeDiagnostics

-- | Generate diagnostic report (public API for CLI integration)
generateDiagnosticReport :: GlobalOptions -> FilePath -> IO (Either CLIDiagnostic ())
generateDiagnosticReport globalOpts reportPath = do
  diagnosticResult <- executeDiagnostics globalOpts
  case diagnosticResult of
    Left diag -> return $ Left diag
    Right result -> do
      -- TODO: Write diagnostic report to file
      return $ Right ()

-- | Identify issues (public API for CLI integration)
identifyIssues :: IO (Either CLIDiagnostic [Issue])
identifyIssues = do
  analysisResult <- performSystemHealthCheck StandardAnalysis
  case analysisResult of
    Left diag -> return $ Left diag
    Right analysis -> return $ Right $ getAllIssuesFromAnalysis analysis