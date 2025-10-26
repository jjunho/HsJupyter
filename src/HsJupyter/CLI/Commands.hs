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

-- | Installation command options supporting Phase 5 custom configuration
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

-- | Top-level CLI commands
data CLICommand
  = InstallCommand GlobalOptions InstallOptions
  | DoctorCommand GlobalOptions             -- TODO: Add DoctorOptions when implementing US2
  | UninstallCommand GlobalOptions          -- TODO: Add UninstallOptions when implementing US3
  | ListCommand GlobalOptions               -- TODO: Add ListOptions when implementing US3
  deriving (Show, Eq)

-- | Parse command line arguments into structured command
parseCommand :: [String] -> Either String CLICommand
parseCommand args = 
  let parser = commandParser
      prefs = defaultPrefs { prefShowHelpOnError = True, prefHelpLongEquals = True }
      result = execParserPure prefs (info parser cmdHelp) args
  in case result of
       Options.Applicative.Success cmd -> Right cmd
       Options.Applicative.Failure failure -> Left $ show failure
       Options.Applicative.CompletionInvoked _ -> Left "Completion invoked"

-- | Main command parser with subcommands
commandParser :: Parser CLICommand
commandParser = subparser $ mconcat
  [ command "install" $ info installCommandParser $ progDesc "Install HsJupyter kernel"
  , command "doctor" $ info doctorCommandParser $ progDesc "Diagnose installation issues"
  , command "uninstall" $ info uninstallCommandParser $ progDesc "Remove HsJupyter kernel"
  , command "list" $ info listCommandParser $ progDesc "List HsJupyter installations"
  ]

-- | Install command parser
installCommandParser :: Parser CLICommand
installCommandParser = InstallCommand <$> globalOptionsParser <*> installOptionsParser

-- | Doctor command parser (placeholder for US2)
doctorCommandParser :: Parser CLICommand
doctorCommandParser = DoctorCommand <$> globalOptionsParser

-- | Uninstall command parser (placeholder for US3)
uninstallCommandParser :: Parser CLICommand
uninstallCommandParser = UninstallCommand <$> globalOptionsParser

-- | List command parser (placeholder for US3)
listCommandParser :: Parser CLICommand
listCommandParser = ListCommand <$> globalOptionsParser

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
    (key, value) | not (T.null value) -> Right (key, T.drop 1 value)
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

-- | Help text for CLI
cmdHelp :: InfoMod a
cmdHelp = briefDesc <> header "hs-jupyter-kernel - Install and manage Haskell Jupyter kernels"