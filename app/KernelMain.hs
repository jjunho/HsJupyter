module Main (main) where


import Data.Char (toLower)
import qualified Data.Text as T
import Options.Applicative
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)

import HsJupyter.KernelProcess
  ( LoadConfigError(..)
  , LogLevel(..)
  , loadKernelProcessConfig
  , runKernel
  , summariseConfig
  )

-- | Application modes: either run kernel server or CLI commands
data AppMode
  = KernelServer KernelOptions    -- Traditional kernel server mode
  | CLICommand [String]           -- CLI subcommand mode (placeholder for now)
  deriving (Show)

-- | Kernel server options (existing functionality)
data KernelOptions = KernelOptions
  { optConnection :: FilePath
  , optLogLevel   :: Maybe LogLevel
  } deriving (Show)

-- | Parse kernel server options
kernelOptionsParser :: Parser KernelOptions
kernelOptionsParser = KernelOptions
  <$> strOption
        ( long "connection"
       <> metavar "FILE"
       <> help "Path to Jupyter connection file"
        )
  <*> optional (option auto
        ( long "log-level"
       <> metavar "LEVEL"
       <> help "Log level (Debug|Info|Warn|Error)"
        ))

-- | Determine application mode based on command line arguments
determineMode :: [String] -> AppMode
determineMode args = case args of
  [] -> KernelServer defaultKernelOptions  -- Default to server mode
  ("install":_) -> CLICommand args
  ("doctor":_) -> CLICommand args
  ("uninstall":_) -> CLICommand args
  ("list":_) -> CLICommand args
  ("version":_) -> CLICommand args
  _ -> KernelServer defaultKernelOptions  -- Default to server mode for unknown args
  where
    defaultKernelOptions = KernelOptions
      { optConnection = ""  -- Will be handled by parser
      , optLogLevel = Nothing
      }

main :: IO ()
main = do
  args <- getArgs
  case determineMode args of
    CLICommand cliArgs -> do
      -- Placeholder for CLI command handling
      -- TODO: Implement CLI command dispatch (will be done in later tasks)
      putStrLn $ "CLI command mode (not yet implemented): " ++ show cliArgs
      putStrLn "Available commands: install, doctor, uninstall, list, version"
      die "CLI commands will be implemented in subsequent tasks"
    
    KernelServer _ -> do
      -- Use existing kernel server logic
      opts <- execParser optsInfo
      envLevelStr <- lookupEnv "HSJUPYTER_LOG_LEVEL"
      let envLevel = envLevelStr >>= parseLogLevel
          effectiveLevel = optLogLevel opts <|> envLevel
      eConfig <- loadKernelProcessConfig (optConnection opts) effectiveLevel
      case eConfig of
        Left err -> die (renderError err)
        Right cfg -> do
          putStrLn $ "[hsjupyter] binding kernel: " <> T.unpack (summariseConfig cfg)
          putStrLn "[hsjupyter] kernel ready – waiting for Jupyter messages (Ctrl+C to exit)"
          runKernel cfg
  where
    optsInfo = info (kernelOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "HsJupyter kernel server and CLI management tools"
     <> header "hs-jupyter-kernel - Haskell kernel for Jupyter with CLI tools"
      )

    renderError :: LoadConfigError -> String
    renderError err = case err of
      ConfigFileMissing path -> "Connection file missing: " <> path
      ConfigDecodeError msg  -> "Failed to parse connection file: " <> msg
      ConfigValidationError msg -> "Connection file invalid: " <> msg

    parseLogLevel :: String -> Maybe LogLevel
    parseLogLevel input = case map toLower input of
      "debug" -> Just LogDebug
      "info"  -> Just LogInfo
      "warn"  -> Just LogWarn
      "error" -> Just LogError
      _       -> Nothing
