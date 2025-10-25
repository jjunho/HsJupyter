# Data Model: Installation & CLI Infrastructure

**Feature**: 004-install-cli  
**Date**: 2025-01-28  
**Status**: Complete

## Core Entities

### JupyterEnvironment

Represents a detected Jupyter installation with configuration details.

```haskell
data JupyterEnvironment = JupyterEnvironment
  { jeKernelspecDirs :: [FilePath]        -- Available kernelspec directories
  , jePythonEnv      :: PythonEnvironment -- Python environment info
  , jeVersion        :: JupyterVersion    -- Jupyter version information
  , jeInstallType    :: InstallationType  -- System/user/conda environment
  } deriving (Show, Eq)

data PythonEnvironment = PythonEnvironment
  { pePath        :: FilePath           -- Python executable path
  , peVersion     :: Text               -- Python version string
  , peEnvironment :: Maybe Text         -- Conda env name or virtualenv path
  } deriving (Show, Eq)

data JupyterVersion = JupyterVersion
  { jvLab      :: Maybe Text           -- JupyterLab version if available
  , jvNotebook :: Maybe Text           -- Jupyter Notebook version if available
  , jvCore     :: Text                 -- Jupyter core version
  } deriving (Show, Eq)

data InstallationType 
  = SystemWide              -- System-wide installation
  | UserLocal               -- User-specific installation
  | CondaEnvironment Text   -- Conda environment installation
  deriving (Show, Eq)
```

### KernelInstallation

Represents an HsJupyter kernel installation with status and configuration.

```haskell
data KernelInstallation = KernelInstallation
  { kiKernelspecPath :: FilePath          -- Path to kernel.json file
  , kiDisplayName    :: Text              -- Display name in Jupyter
  , kiVersion        :: Text              -- HsJupyter kernel version
  , kiGHCPath        :: FilePath          -- GHC executable path
  , kiStatus         :: InstallationStatus -- Current installation status
  , kiConfiguration  :: KernelConfig      -- Kernel configuration details
  } deriving (Show, Eq)

data InstallationStatus
  = Installed                    -- Properly installed and functional
  | InstalledWithIssues [Issue]  -- Installed but has problems
  | NotInstalled                 -- No installation found
  | Corrupted [Issue]            -- Installation exists but corrupted
  deriving (Show, Eq)

data KernelConfig = KernelConfig
  { kcResourceLimits :: ResourceLimits    -- Memory/CPU/timeout limits
  , kcDisplayName    :: Text              -- Kernel display name
  , kcLanguage       :: Text              -- Language identifier ("haskell")
  , kcInterruptMode  :: InterruptMode     -- Signal/message interrupt handling
  , kcMetadata       :: Value             -- Additional kernel metadata
  } deriving (Show, Eq)

data ResourceLimits = ResourceLimits
  { rlMemoryLimitMB    :: Maybe Int       -- Memory limit in MB
  , rlTimeoutSeconds   :: Maybe Int       -- Execution timeout in seconds
  , rlMaxOutputSizeKB  :: Maybe Int       -- Maximum output size in KB
  } deriving (Show, Eq)

data InterruptMode = Signal | Message
  deriving (Show, Eq)
```

### InstallationConfiguration

Represents user-specified installation parameters and options.

```haskell
data InstallationConfiguration = InstallationConfiguration
  { icInstallScope     :: InstallScope        -- Installation scope selection
  , icCustomPaths      :: Maybe CustomPaths   -- Custom path overrides
  , icKernelConfig     :: Maybe KernelConfig  -- Custom kernel configuration
  , icForceReinstall   :: Bool                -- Force overwrite existing
  , icQuietMode        :: Bool                -- Suppress interactive prompts
  , icValidationLevel  :: ValidationLevel    -- Installation validation depth
  } deriving (Show, Eq)

data InstallScope
  = AutoDetect              -- Automatically choose best scope
  | UserInstallation        -- Install for current user only
  | SystemInstallation      -- Install system-wide (requires permissions)
  | CustomPath FilePath     -- Install to specific directory
  deriving (Show, Eq)

data CustomPaths = CustomPaths
  { cpJupyterDir     :: Maybe FilePath    -- Custom Jupyter directory
  , cpKernelspecDir  :: Maybe FilePath    -- Custom kernelspec directory
  , cpGHCPath        :: Maybe FilePath    -- Custom GHC executable path
  } deriving (Show, Eq)

data ValidationLevel
  = NoValidation          -- Skip validation (fastest)
  | BasicValidation       -- Verify files exist and are readable
  | FullValidation        -- Test kernel functionality
  deriving (Show, Eq)
```

### DiagnosticResult

Represents diagnostic and troubleshooting information.

```haskell
data DiagnosticResult = DiagnosticResult
  { drOverallStatus   :: HealthStatus         -- Overall system health
  , drJupyterStatus   :: JupyterStatus        -- Jupyter installation status
  , drKernelStatus    :: KernelStatus         -- HsJupyter kernel status
  , drIssuesFound     :: [Issue]              -- List of identified issues
  , drRecommendations :: [Recommendation]     -- Suggested actions
  , drSystemInfo      :: SystemInformation    -- System environment details
  } deriving (Show, Eq)

data HealthStatus
  = Healthy                    -- Everything working correctly
  | HealthyWithWarnings        -- Working but has minor issues
  | Degraded                   -- Partially functional
  | Broken                     -- Not functional
  deriving (Show, Eq)

data JupyterStatus = JupyterStatus
  { jsInstalled       :: Bool              -- Jupyter is installed
  , jsVersion         :: Maybe Text        -- Version information
  , jsKernelspecDirs  :: [FilePath]        -- Available kernelspec directories
  , jsAccessible      :: Bool              -- Can write to kernelspec dirs
  } deriving (Show, Eq)

data KernelStatus = KernelStatus
  { ksInstalled       :: Bool              -- HsJupyter kernel is installed
  , ksVersion         :: Maybe Text        -- Kernel version
  , ksFunctional      :: Bool              -- Kernel can execute code
  , ksGHCAvailable    :: Bool              -- GHC is accessible
  } deriving (Show, Eq)

data Issue = Issue
  { iSeverity     :: Severity              -- Issue severity level
  , iComponent    :: Component             -- Affected component
  , iDescription  :: Text                  -- Human-readable description
  , iDetails      :: Maybe Text            -- Additional technical details
  } deriving (Show, Eq)

data Severity = Critical | Major | Minor | Warning
  deriving (Show, Eq, Ord)

data Component 
  = JupyterComponent       -- Jupyter installation issues
  | KernelComponent        -- HsJupyter kernel issues
  | GHCComponent           -- GHC/Haskell toolchain issues
  | SystemComponent        -- System-level issues (permissions, paths)
  deriving (Show, Eq)

data Recommendation = Recommendation
  { rPriority     :: Priority              -- Recommendation priority
  , rAction       :: Text                  -- Recommended action
  , rCommand      :: Maybe Text            -- Specific command to run
  , rRationale    :: Text                  -- Why this recommendation helps
  } deriving (Show, Eq)

data Priority = Immediate | High | Medium | Low
  deriving (Show, Eq, Ord)

data SystemInformation = SystemInformation
  { siPlatform        :: Text              -- Operating system platform
  , siArchitecture    :: Text              -- CPU architecture
  , siShell           :: Maybe Text        -- Shell environment
  , siPATH            :: [FilePath]        -- System PATH variable
  , siWorkingDir      :: FilePath          -- Current working directory
  } deriving (Show, Eq)
```

## Entity Relationships

```text
JupyterEnvironment -> contains multiple -> KernelInstallation
InstallationConfiguration -> configures -> KernelInstallation
DiagnosticResult -> analyzes -> JupyterEnvironment + KernelInstallation
Issue -> identified within -> DiagnosticResult
Recommendation -> suggests actions for -> Issue
```

## State Transitions

### Installation Status Transitions

```text
NotInstalled 
  -> install command -> Installed | InstalledWithIssues | Corrupted

InstalledWithIssues 
  -> doctor command -> identifies issues
  -> install --force -> Installed | Corrupted

Corrupted 
  -> uninstall -> NotInstalled
  -> install --force -> Installed | InstalledWithIssues

Installed 
  -> uninstall -> NotInstalled
  -> install --force -> Installed (version update)
```

### Health Status Transitions

```text
Healthy -> validation detects issues -> HealthyWithWarnings | Degraded | Broken
HealthyWithWarnings -> fix applied -> Healthy
Degraded -> repair actions -> Healthy | HealthyWithWarnings | Broken
Broken -> reinstallation -> Healthy | Degraded
```

## Validation Rules

### KernelInstallation Validation

- `kiKernelspecPath` MUST point to valid kernel.json file
- `kiGHCPath` MUST be executable and accessible
- `kiDisplayName` MUST be non-empty and valid for Jupyter display
- `kiVersion` MUST match actual kernel executable version

### InstallationConfiguration Validation

- `icCustomPaths.cpGHCPath` if provided MUST be executable
- `icCustomPaths.cpKernelspecDir` if provided MUST be writable
- `icInstallScope` MUST be compatible with available permissions
- Resource limits in `icKernelConfig` MUST be positive values

### DiagnosticResult Validation

- `drIssuesFound` MUST be sorted by severity (Critical first)
- `drRecommendations` MUST be sorted by priority (Immediate first)
- Each `Issue` MUST have corresponding `Recommendation` when actionable
- `drOverallStatus` MUST reflect worst issue severity found

## Integration Points

### Constitutional Error Handling

All data models integrate with existing `RuntimeDiagnostic` system:

```haskell
-- CLI-specific errors extend existing pattern
data CLIDiagnostic
  = InstallationError Issue
  | ValidationError Text
  | ConfigurationError Text
  | SystemIntegrationError Text
  deriving (Show, Eq)

instance ToDiagnostic CLIDiagnostic where
  toDiagnostic (InstallationError issue) = 
    RuntimeDiagnostic { ... }  -- Maps to constitutional error format
```

### Observability Integration

All entities support structured logging through existing katip infrastructure:

```haskell
instance ToObject KernelInstallation where
  toObject ki = fromList
    [ "kernelspec_path" .= kiKernelspecPath ki
    , "version" .= kiVersion ki
    , "status" .= show (kiStatus ki)
    ]
```

### Resource Management

Integrates with existing `ResourceGuard` for installation operations:

```haskell
-- Installation operations respect constitutional resource limits
installWithGuard :: ResourceGuard -> InstallationConfiguration -> IO (Either CLIDiagnostic KernelInstallation)
```
