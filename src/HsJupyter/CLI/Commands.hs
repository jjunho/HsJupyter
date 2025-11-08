{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | CLI Commands module - handles command-line interface parsing and routing
module HsJupyter.CLI.Commands 
  ( CLICommand(..)
  , InstallOptions(..)
  , GlobalOptions(..)
  , parseCommand
  , defaultInstallOptions
  , defaultGlobalOptions
  -- T037: List and Version types exports
  , ListOptions(..)
  , VersionOptions(..)
  -- T038: Uninstall types exports
  , UninstallOptions(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

import HsJupyter.CLI.Types 
  ( InstallScope(..)
  , ValidationLevel(..)
  , ResourceLimits(..)
  )

-- | Global CLI options available for all commands
data GlobalOptions = GlobalOptions
  { goJSON    :: Bool        -- ^ Output structured JSON
  , goQuiet   :: Bool        -- ^ Suppress non-essential output
  , goVerbose :: Bool        -- ^ Enable detailed logging
  } deriving (Show, Eq)

-- | Default global options following constitutional KISS principles
defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions
  { goJSON = False           -- Human-readable by default
  , goQuiet = False          -- Interactive by default
  , goVerbose = False        -- Standard logging level
  }

-- | Installation command options supporting Phase 5 custom configuration + US4 non-interactive
data InstallOptions = InstallOptions
  { ioScope            :: InstallScope     -- ^ Installation scope (user/system/auto)
  , ioForceReinstall   :: Bool             -- ^ Force overwrite existing installation
  , ioValidationLevel  :: ValidationLevel -- ^ Post-install validation depth
  , ioDisplayName      :: Maybe Text       -- ^ Custom kernel display name
  , ioGHCPath          :: Maybe FilePath   -- ^ Custom GHC executable path
  , ioJupyterDir       :: Maybe FilePath   -- ^ Custom Jupyter directory
  , ioKernelspecDir    :: Maybe FilePath   -- ^ Custom kernelspec directory
  -- Phase 5 US3 extensions: Custom configuration support
  , ioConfigFile       :: Maybe FilePath   -- ^ Custom configuration file path
  , ioLanguage         :: Maybe Text       -- ^ Custom kernel language identifier
  , ioEnvironmentVars  :: [(Text, Text)]   -- ^ Additional environment variables
  , ioKernelArguments  :: [Text]           -- ^ Additional kernel startup arguments
  , ioResourceLimits   :: Maybe ResourceLimits -- ^ Custom resource limits
  , ioConnectionTimeout :: Maybe Int       -- ^ Custom connection timeout (seconds)
  } deriving (Show, Eq)

-- | Doctor command options (Phase 6 US4)
data DoctorOptions = DoctorOptions
  { doCheckComponents :: [ComponentType]  -- ^ Specific components to check
  , doFixIssues       :: Bool             -- ^ Attempt automatic fixes
  , doReportFile      :: Maybe FilePath   -- ^ Save detailed report to file
  } deriving (Show, Eq)

-- | Uninstall command options (Phase 6 US4)
data UninstallOptions = UninstallOptions
  { uoRemoveAll       :: Bool             -- ^ Remove all installations
  , uoKernelspecDir   :: Maybe FilePath   -- ^ Remove from specific directory
  , uoConfirm         :: Bool             -- ^ Skip confirmation prompts
  -- T038: Additional options for comprehensive uninstall
  , uoForce           :: Bool             -- ^ Force removal even if issues detected
  , uoCleanupAll      :: Bool             -- ^ Perform global cleanup
  , uoRemoveConfig    :: Bool             -- ^ Remove configuration files
  , uoRemoveLogs      :: Bool             -- ^ Remove log files
  } deriving (Show, Eq)

-- | List command options (Phase 6 US4)
data ListOptions = ListOptions
  { loShowAll         :: Bool             -- ^ Include non-functional installations
  , loSearchPath      :: Maybe FilePath   -- ^ Search specific directory
  } deriving (Show, Eq)

-- | Version command options (Phase 6 US4)
data VersionOptions = VersionOptions
  { voCheckCompatibility :: Bool          -- ^ Check system compatibility
  } deriving (Show, Eq)

-- | Component types for doctor command
data ComponentType
  = JupyterComp       -- ^ Jupyter installation
  | KernelComp        -- ^ HsJupyter kernel
  | GHCComp           -- ^ GHC toolchain
  | SystemComp        -- ^ System environment
  | AllComps          -- ^ All components
  deriving (Show, Eq)

-- | Default installation options following constitutional KISS principle
defaultInstallOptions :: InstallOptions
defaultInstallOptions = InstallOptions
  { ioScope = AutoDetect              -- Auto-detect best installation scope
  , ioForceReinstall = False          -- Safe default - don't overwrite
  , ioValidationLevel = BasicValidation -- Balance speed vs verification
  , ioDisplayName = Nothing           -- Use default "Haskell" name
  , ioGHCPath = Nothing               -- Auto-detect GHC
  , ioJupyterDir = Nothing            -- Use standard Jupyter directories
  , ioKernelspecDir = Nothing         -- Use standard kernelspec directories
  -- Phase 5 US3 defaults: Safe, minimal configuration
  , ioConfigFile = Nothing            -- Use default config discovery
  , ioLanguage = Nothing              -- Use default "haskell" language
  , ioEnvironmentVars = []            -- No additional environment variables
  , ioKernelArguments = []            -- No additional kernel arguments
  , ioResourceLimits = Nothing        -- Use system defaults
  , ioConnectionTimeout = Nothing     -- Use Jupyter default timeout
  }

-- Note: Default option values are defined here for documentation purposes
-- but are not exported. They serve as reference for the option parsers below.

-- | Parse CLI commands

-- | Top-level CLI commands with Phase 6 US4 enhancements
data CLICommand
  = InstallCommand GlobalOptions InstallOptions
  | DoctorCommand GlobalOptions DoctorOptions
  | UninstallCommand GlobalOptions UninstallOptions
  | ListCommand GlobalOptions ListOptions
  | VersionCommand GlobalOptions VersionOptions
  deriving (Show, Eq)

-- | Parse command line arguments into structured command
parseCommand :: [String] -> Either String CLICommand
parseCommand args = 
  let parser = commandParser
      customPrefs = defaultPrefs { prefShowHelpOnError = True, prefHelpLongEquals = True }
      result = execParserPure customPrefs (info parser cmdHelp) args
  in case result of
       Options.Applicative.Success cmd -> Right cmd
       Options.Applicative.Failure failure -> Left $ show failure
       Options.Applicative.CompletionInvoked _ -> Left "Completion invoked"

-- | Main command parser with subcommands (Phase 6 US4 complete)
commandParser :: Parser CLICommand
commandParser = subparser $ mconcat
  [ command "install" $ info installCommandParser $ progDesc "Install HsJupyter kernel"
  , command "doctor" $ info doctorCommandParser $ progDesc "Diagnose installation issues"
  , command "uninstall" $ info uninstallCommandParser $ progDesc "Remove HsJupyter kernel"
  , command "list" $ info listCommandParser $ progDesc "List HsJupyter installations"
  , command "version" $ info versionCommandParser $ progDesc "Show version information"
  ]

-- | Install command parser
installCommandParser :: Parser CLICommand
installCommandParser = InstallCommand <$> globalOptionsParser <*> installOptionsParser

-- | Doctor command parser (Phase 6 US4 implementation)
doctorCommandParser :: Parser CLICommand
doctorCommandParser = DoctorCommand <$> globalOptionsParser <*> doctorOptionsParser

-- | Uninstall command parser (Phase 6 US4 implementation)
uninstallCommandParser :: Parser CLICommand
uninstallCommandParser = UninstallCommand <$> globalOptionsParser <*> uninstallOptionsParser

-- | List command parser (Phase 6 US4 implementation)
listCommandParser :: Parser CLICommand
listCommandParser = ListCommand <$> globalOptionsParser <*> listOptionsParser

-- | Version command parser (Phase 6 US4 implementation)
versionCommandParser :: Parser CLICommand
versionCommandParser = VersionCommand <$> globalOptionsParser <*> versionOptionsParser

-- | Global options parser
globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
  <$> switch (long "json" <> help "Output structured JSON")
  <*> switch (long "quiet" <> help "Suppress non-essential output")
  <*> switch (long "verbose" <> help "Enable detailed logging")

-- | Installation options parser with Phase 5 US3 extensions
installOptionsParser :: Parser InstallOptions
installOptionsParser = InstallOptions
  <$> installScopeParser
  <*> switch (long "force" <> help "Force overwrite existing installation")
  <*> validationLevelParser
  <*> optional (strOption (long "display-name" <> metavar "NAME" <> help "Custom kernel display name"))
  <*> optional (strOption (long "ghc-path" <> metavar "PATH" <> help "Custom GHC executable path"))
  <*> optional (strOption (long "jupyter-dir" <> metavar "DIR" <> help "Custom Jupyter directory"))
  <*> optional (strOption (long "kernelspec-dir" <> metavar "DIR" <> help "Custom kernelspec directory"))
  -- Phase 5 US3 custom configuration options
  <*> optional (strOption (long "config" <> metavar "FILE" <> help "Custom configuration file"))
  <*> optional (strOption (long "language" <> metavar "LANG" <> help "Custom kernel language identifier"))
  <*> environmentVarsParser
  <*> kernelArgumentsParser
  <*> resourceLimitsParser
  <*> optional (option auto (long "timeout" <> metavar "SECONDS" <> help "Connection timeout in seconds"))

-- | Installation scope parser
installScopeParser :: Parser InstallScope
installScopeParser = 
  flag AutoDetect UserInstallation (long "user" <> help "Install for current user only")
  <|> flag AutoDetect SystemInstallation (long "system" <> help "Install system-wide")

-- | Validation level parser
validationLevelParser :: Parser ValidationLevel
validationLevelParser = option validationLevelReader $
  long "validation" <> metavar "LEVEL" <> value BasicValidation <> help "Validation level (none|basic|full)"

-- | Reader for validation levels
validationLevelReader :: ReadM ValidationLevel
validationLevelReader = eitherReader $ \case
  "none" -> Right NoValidation
  "basic" -> Right BasicValidation
  "full" -> Right FullValidation
  invalid -> Left $ "Invalid validation level: " ++ invalid

-- | Environment variables parser (Phase 5 US3)
environmentVarsParser :: Parser [(Text, Text)]
environmentVarsParser = many $ option envVarReader $
  long "env" <> metavar "KEY=VALUE" <> help "Additional environment variable (can be specified multiple times)"

-- | Environment variable reader
envVarReader :: ReadM (Text, Text)
envVarReader = eitherReader $ \s ->
  case T.breakOn "=" (T.pack s) of
    (key, val) | not (T.null val) -> Right (key, T.drop 1 val)
    _ -> Left $ "Invalid environment variable format. Expected KEY=VALUE, got: " ++ s

-- | Kernel arguments parser (Phase 5 US3)
kernelArgumentsParser :: Parser [Text]
kernelArgumentsParser = many $ strOption $
  long "kernel-arg" <> metavar "ARG" <> help "Additional kernel startup argument (can be specified multiple times)"

-- | Resource limits parser (Phase 5 US3)
resourceLimitsParser :: Parser (Maybe ResourceLimits)
resourceLimitsParser = optional $ ResourceLimits
  <$> optional (option auto (long "memory-limit" <> metavar "MB" <> help "Memory limit in MB"))
  <*> optional (option auto (long "exec-timeout" <> metavar "SECONDS" <> help "Execution timeout in seconds"))
  <*> optional (option auto (long "output-limit" <> metavar "KB" <> help "Maximum output size in KB"))

-- | Doctor options parser (Phase 6 US4)
doctorOptionsParser :: Parser DoctorOptions
doctorOptionsParser = DoctorOptions
  <$> many (componentTypeParser)
  <*> switch (long "fix" <> help "Attempt to automatically fix detected issues")
  <*> optional (strOption (long "report" <> metavar "FILE" <> help "Save detailed diagnostic report to file"))

-- | Component type parser for doctor command
componentTypeParser :: Parser ComponentType
componentTypeParser = option componentReader $
  long "check" <> metavar "COMPONENT" <> help "Check specific component (jupyter|kernel|ghc|system|all)"

-- | Component type reader
componentReader :: ReadM ComponentType
componentReader = eitherReader $ \case
  "jupyter" -> Right JupyterComp
  "kernel" -> Right KernelComp
  "ghc" -> Right GHCComp
  "system" -> Right SystemComp
  "all" -> Right AllComps
  invalid -> Left $ "Invalid component type: " ++ invalid

-- | Uninstall options parser (Phase 6 US4)
uninstallOptionsParser :: Parser UninstallOptions
uninstallOptionsParser = UninstallOptions
  <$> switch (long "all" <> help "Remove all HsJupyter kernel installations found")
  <*> optional (strOption (long "kernelspec-dir" <> metavar "DIR" <> help "Remove from specific kernelspec directory"))
  <*> switch (long "confirm" <> help "Skip confirmation prompts (use with caution)")
  -- T038: Additional uninstall options
  <*> switch (long "force" <> help "Force removal even if installation has issues")
  <*> switch (long "cleanup-all" <> help "Perform global cleanup of temporary files")
  <*> switch (long "remove-config" <> help "Remove configuration files")
  <*> switch (long "remove-logs" <> help "Remove log files")

-- | List options parser (Phase 6 US4)
listOptionsParser :: Parser ListOptions
listOptionsParser = ListOptions
  <$> switch (long "all" <> help "Include non-functional and problematic installations")
  <*> optional (strOption (long "path" <> metavar "DIR" <> help "Search specific directory for installations"))

-- | Version options parser (Phase 6 US4)
versionOptionsParser :: Parser VersionOptions
versionOptionsParser = VersionOptions
  <$> switch (long "check-compatibility" <> help "Check compatibility with current system")

-- | Help text for CLI
cmdHelp :: InfoMod a
cmdHelp = briefDesc <> header "hs-jupyter-kernel - Install and manage Haskell Jupyter kernels"

