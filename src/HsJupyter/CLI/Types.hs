{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.CLI.Types where

import Data.Aeson (Value, ToJSON(..), FromJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic(..), DiagnosticSeverity(..), mkDiagnostic)

-- | CLI-specific diagnostic errors that extend the constitutional RuntimeDiagnostic system
data CLIDiagnostic
  = InstallationError Issue
  | ValidationError Text
  | ConfigurationError Text
  | SystemIntegrationError Text
  deriving (Show, Eq, Generic)

instance ToJSON CLIDiagnostic

-- | Convert CLI diagnostics to constitutional RuntimeDiagnostic format
instance {-# OVERLAPPABLE #-} ToDiagnostic CLIDiagnostic where
  toDiagnostic (InstallationError issue) = 
    mkDiagnostic (severityToRuntimeSeverity $ iSeverity issue) (iDescription issue)
  toDiagnostic (ValidationError msg) = 
    mkDiagnostic SeverityError msg
  toDiagnostic (ConfigurationError msg) = 
    mkDiagnostic SeverityError msg
  toDiagnostic (SystemIntegrationError msg) = 
    mkDiagnostic SeverityError msg

-- | Map CLI severity to runtime severity
severityToRuntimeSeverity :: Severity -> DiagnosticSeverity
severityToRuntimeSeverity Critical = SeverityError
severityToRuntimeSeverity Major = SeverityError
severityToRuntimeSeverity Minor = SeverityWarning
severityToRuntimeSeverity Warning = SeverityWarning

-- | Core data types for CLI operations

-- | Represents a detected Jupyter installation with configuration details
data JupyterEnvironment = JupyterEnvironment
  { jeKernelspecDirs :: [FilePath]        -- ^ Available kernelspec directories
  , jePythonEnv      :: PythonEnvironment -- ^ Python environment info
  , jeVersion        :: JupyterVersion    -- ^ Jupyter version information
  , jeInstallType    :: InstallationType  -- ^ System/user/conda environment
  } deriving (Show, Eq, Generic)

instance ToJSON JupyterEnvironment
instance FromJSON JupyterEnvironment

data PythonEnvironment = PythonEnvironment
  { pePath        :: FilePath           -- ^ Python executable path
  , peVersion     :: Text               -- ^ Python version string
  , peEnvironment :: Maybe Text         -- ^ Conda env name or virtualenv path
  } deriving (Show, Eq, Generic)

instance ToJSON PythonEnvironment
instance FromJSON PythonEnvironment

data JupyterVersion = JupyterVersion
  { jvLab      :: Maybe Text           -- ^ JupyterLab version if available
  , jvNotebook :: Maybe Text           -- ^ Jupyter Notebook version if available
  , jvCore     :: Text                 -- ^ Jupyter core version
  } deriving (Show, Eq, Generic)

instance ToJSON JupyterVersion
instance FromJSON JupyterVersion

data InstallationType 
  = SystemWide              -- ^ System-wide installation
  | UserLocal               -- ^ User-specific installation
  | CondaEnvironment Text   -- ^ Conda environment installation
  deriving (Show, Eq, Generic)

instance ToJSON InstallationType
instance FromJSON InstallationType

-- | Represents an HsJupyter kernel installation with status and configuration
data KernelInstallation = KernelInstallation
  { kiKernelspecPath :: FilePath          -- ^ Path to kernel.json file
  , kiDisplayName    :: Text              -- ^ Display name in Jupyter
  , kiVersion        :: Text              -- ^ HsJupyter kernel version
  , kiGHCPath        :: FilePath          -- ^ GHC executable path
  , kiStatus         :: InstallationStatus -- ^ Current installation status
  , kiConfiguration  :: KernelConfig      -- ^ Kernel configuration details
  } deriving (Show, Eq, Generic)

instance ToJSON KernelInstallation

data InstallationStatus
  = Installed                    -- ^ Properly installed and functional
  | InstalledWithIssues [Issue]  -- ^ Installed but has problems
  | NotInstalled                 -- ^ No installation found
  | Corrupted [Issue]            -- ^ Installation exists but corrupted
  deriving (Show, Eq, Generic)

instance ToJSON InstallationStatus

data KernelConfig = KernelConfig
  { kcResourceLimits :: ResourceLimits    -- ^ Memory/CPU/timeout limits
  , kcDisplayName    :: Text              -- ^ Kernel display name
  , kcLanguage       :: Text              -- ^ Language identifier ("haskell")
  , kcInterruptMode  :: InterruptMode     -- ^ Signal/message interrupt handling
  , kcMetadata       :: Value             -- ^ Additional kernel metadata
  } deriving (Show, Eq, Generic)

instance ToJSON KernelConfig

data ResourceLimits = ResourceLimits
  { rlMemoryLimitMB    :: Maybe Int       -- ^ Memory limit in MB
  , rlTimeoutSeconds   :: Maybe Int       -- ^ Execution timeout in seconds
  , rlMaxOutputSizeKB  :: Maybe Int       -- ^ Maximum output size in KB
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceLimits

data InterruptMode = Signal | Message
  deriving (Show, Eq, Generic)

instance ToJSON InterruptMode

-- | Represents user-specified installation parameters and options
data InstallationConfiguration = InstallationConfiguration
  { icInstallScope     :: InstallScope        -- ^ Installation scope selection
  , icCustomPaths      :: Maybe CustomPaths   -- ^ Custom path overrides
  , icKernelConfig     :: Maybe KernelConfig  -- ^ Custom kernel configuration
  , icForceReinstall   :: Bool                -- ^ Force overwrite existing
  , icQuietMode        :: Bool                -- ^ Suppress interactive prompts
  , icValidationLevel  :: ValidationLevel    -- ^ Installation validation depth
  } deriving (Show, Eq, Generic)

instance ToJSON InstallationConfiguration

data InstallScope
  = AutoDetect              -- ^ Automatically choose best scope
  | UserInstallation        -- ^ Install for current user only
  | SystemInstallation      -- ^ Install system-wide (requires permissions)
  | CustomPath FilePath     -- ^ Install to specific directory
  deriving (Show, Eq, Generic)

instance ToJSON InstallScope

data CustomPaths = CustomPaths
  { cpJupyterDir     :: Maybe FilePath    -- ^ Custom Jupyter directory
  , cpKernelspecDir  :: Maybe FilePath    -- ^ Custom kernelspec directory
  , cpGHCPath        :: Maybe FilePath    -- ^ Custom GHC executable path
  } deriving (Show, Eq, Generic)

instance ToJSON CustomPaths

data ValidationLevel
  = NoValidation          -- ^ Skip validation (fastest)
  | BasicValidation       -- ^ Verify files exist and are readable
  | FullValidation        -- ^ Test kernel functionality
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ValidationLevel

-- | Represents diagnostic and troubleshooting information
data DiagnosticResult = DiagnosticResult
  { drOverallStatus   :: HealthStatus         -- ^ Overall system health
  , drJupyterStatus   :: JupyterStatus        -- ^ Jupyter installation status
  , drKernelStatus    :: KernelStatus         -- ^ HsJupyter kernel status
  , drIssuesFound     :: [Issue]              -- ^ List of identified issues
  , drRecommendations :: [Recommendation]     -- ^ Suggested actions
  , drSystemInfo      :: SystemInformation    -- ^ System environment details
  } deriving (Show, Eq, Generic)

instance ToJSON DiagnosticResult

data HealthStatus
  = Healthy                    -- ^ Everything working correctly
  | HealthyWithWarnings        -- ^ Working but has minor issues
  | Degraded                   -- ^ Partially functional
  | Broken                     -- ^ Not functional
  deriving (Show, Eq, Generic)

instance ToJSON HealthStatus

data JupyterStatus = JupyterStatus
  { jsInstalled       :: Bool              -- ^ Jupyter is installed
  , jsVersion         :: Maybe Text        -- ^ Version information
  , jsKernelspecDirs  :: [FilePath]        -- ^ Available kernelspec directories
  , jsAccessible      :: Bool              -- ^ Can write to kernelspec dirs
  } deriving (Show, Eq, Generic)

instance ToJSON JupyterStatus

data KernelStatus = KernelStatus
  { ksInstalled       :: Bool              -- ^ HsJupyter kernel is installed
  , ksVersion         :: Maybe Text        -- ^ Kernel version
  , ksFunctional      :: Bool              -- ^ Kernel can execute code
  , ksGHCAvailable    :: Bool              -- ^ GHC is accessible
  } deriving (Show, Eq, Generic)

instance ToJSON KernelStatus

data Issue = Issue
  { iSeverity     :: Severity              -- ^ Issue severity level
  , iComponent    :: Component             -- ^ Affected component
  , iDescription  :: Text                  -- ^ Human-readable description
  , iDetails      :: Maybe Text            -- ^ Additional technical details
  } deriving (Show, Eq, Generic)

instance ToJSON Issue

data Severity = Critical | Major | Minor | Warning
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Severity

data Component 
  = JupyterComponent       -- ^ Jupyter installation issues
  | KernelComponent        -- ^ HsJupyter kernel issues
  | GHCComponent           -- ^ GHC/Haskell toolchain issues
  | SystemComponent        -- ^ System-level issues (permissions, paths)
  deriving (Show, Eq, Generic)

instance ToJSON Component

data Recommendation = Recommendation
  { rPriority     :: Priority              -- ^ Recommendation priority
  , rAction       :: Text                  -- ^ Recommended action
  , rCommand      :: Maybe Text            -- ^ Specific command to run
  , rRationale    :: Text                  -- ^ Why this recommendation helps
  } deriving (Show, Eq, Generic)

instance ToJSON Recommendation

data Priority = Immediate | High | Medium | Low
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Priority

data SystemInformation = SystemInformation
  { siPlatform        :: Text              -- ^ Operating system platform
  , siArchitecture    :: Text              -- ^ CPU architecture
  , siShell           :: Maybe Text        -- ^ Shell environment
  , siPATH            :: [FilePath]        -- ^ System PATH variable
  , siWorkingDir      :: FilePath          -- ^ Current working directory
  } deriving (Show, Eq, Generic)

instance ToJSON SystemInformation

-- | Type class for converting CLI diagnostics to constitutional format
class ToDiagnostic a where
  toDiagnostic :: a -> RuntimeDiagnostic