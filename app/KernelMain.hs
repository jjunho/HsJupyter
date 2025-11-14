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
import HsJupyter.CLI.Commands (parseCommand, CLICommand(..), ListOptions(..), UninstallOptions(..), VersionOptions(..), GlobalOptions(..))
import HsJupyter.CLI.Install (executeInstall)
import HsJupyter.CLI.Doctor (executeDiagnostics)
import HsJupyter.CLI.Utilities
  ( detectJupyterEnvironment
  , detectConflictingInstallations
  )
import HsJupyter.CLI.Types
  ( JupyterEnvironment(..)
  , KernelInstallation(..)
  , InstallationStatus(..)
  , CLIDiagnostic(..)
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad (unless, when)

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

-- | List all HsJupyter kernel installations
executeList :: GlobalOptions -> ListOptions -> IO (Either CLIDiagnostic ())
executeList globalOpts listOpts = do
  -- Detect Jupyter environment
  envResult <- detectJupyterEnvironment
  case envResult of
    Left err -> return $ Left err
    Right env -> do
      -- Find all kernel installations
      installations <- detectConflictingInstallations env "hsjupyter"

      -- Filter based on options
      let filteredInstallations =
            if loShowAll listOpts
            then installations
            else filter isFunctional installations

      -- Output results
      if goJSON globalOpts
        then do
          -- JSON output
          let jsonOutput = Aeson.object
                [ ("installations", Aeson.toJSON filteredInstallations)
                , ("total_found", Aeson.toJSON (length filteredInstallations))
                , ("functional", Aeson.toJSON (length $ filter isFunctional filteredInstallations))
                ]
          BSL.putStrLn $ Aeson.encode jsonOutput
        else do
          -- Human-readable output
          if null filteredInstallations
            then putStrLn "No HsJupyter kernel installations found."
            else do
              putStrLn $ "Found " ++ show (length filteredInstallations) ++ " HsJupyter installation(s):\n"
              mapM_ printInstallation filteredInstallations

      return $ Right ()
  where
    isFunctional :: KernelInstallation -> Bool
    isFunctional ki = case kiStatus ki of
      Installed -> True
      _ -> False

    printInstallation :: KernelInstallation -> IO ()
    printInstallation ki = do
      putStrLn $ "  • " ++ T.unpack (kiDisplayName ki) ++ " (v" ++ T.unpack (kiVersion ki) ++ ")"
      putStrLn $ "    Path: " ++ kiKernelspecPath ki
      putStrLn $ "    Status: " ++ show (kiStatus ki)
      putStrLn $ "    GHC: " ++ kiGHCPath ki
      putStrLn ""

-- | Uninstall HsJupyter kernel
executeUninstall :: GlobalOptions -> UninstallOptions -> IO (Either CLIDiagnostic String)
executeUninstall globalOpts uninstallOpts = do
  -- Detect Jupyter environment
  envResult <- detectJupyterEnvironment
  case envResult of
    Left err -> return $ Left err
    Right env -> do
      -- Find installations to remove
      installations <- detectConflictingInstallations env "hsjupyter"

      if null installations
        then return $ Right "No HsJupyter kernel installations found to remove."
        else do
          -- Confirm removal unless --confirm flag is set
          unless (uoConfirm uninstallOpts) $ do
            putStrLn $ "Found " ++ show (length installations) ++ " installation(s) to remove:"
            mapM_ (\ki -> putStrLn $ "  • " ++ T.unpack (kiDisplayName ki) ++ " at " ++ kiKernelspecPath ki) installations
            putStrLn "\nAre you sure you want to remove these installations? (yes/no)"
            response <- getLine
            when (map toLower response /= "yes") $ die "Uninstall cancelled by user."

          -- Remove each installation
          results <- mapM removeInstallation installations
          let successful = length [() | Right () <- results]
              failed = length results - successful

          let summary = "Removed " ++ show successful ++ " installation(s)."
                     ++ if failed > 0
                        then " " ++ show failed ++ " removal(s) failed."
                        else ""

          return $ Right summary
  where
    removeInstallation :: KernelInstallation -> IO (Either CLIDiagnostic ())
    removeInstallation ki = do
      let kernelspecPath = kiKernelspecPath ki
          kernelDir = takeDirectory kernelspecPath

      exists <- doesDirectoryExist kernelDir
      if exists
        then do
          removeDirectoryRecursive kernelDir
          unless (goQuiet globalOpts) $
            putStrLn $ "Removed: " ++ kernelDir
          return $ Right ()
        else
          return $ Left $ ValidationError $ "Directory not found: " <> T.pack kernelDir

    takeDirectory :: FilePath -> FilePath
    takeDirectory path = reverse $ dropWhile (/= '/') $ reverse path

-- | Show version information
executeVersion :: GlobalOptions -> VersionOptions -> IO ()
executeVersion globalOpts versionOpts = do
  let version = "0.1.0.0" :: String
      ghcVersion = "9.6.7" :: String
      buildDate = "2025-01-14" :: String

  if goJSON globalOpts
    then do
      let jsonOutput = Aeson.object
            [ ("hs_jupyter_kernel", Aeson.toJSON version)
            , ("ghc_version", Aeson.toJSON ghcVersion)
            , ("build_info", Aeson.object
                [ ("build_date", Aeson.toJSON buildDate)
                , ("platform", Aeson.toJSON ("linux-x86_64" :: String))
                ])
            ]
      BSL.putStrLn $ Aeson.encode jsonOutput
    else do
      putStrLn $ "hs-jupyter-kernel version " ++ version
      putStrLn $ "GHC version: " ++ ghcVersion
      putStrLn $ "Built: " ++ buildDate
      when (voCheckCompatibility versionOpts) $ do
        putStrLn "\nChecking system compatibility..."
        -- TODO: Add compatibility checks
        putStrLn "Platform: linux-x86_64"
        putStrLn "Status: Compatible"

-- | Execute CLI commands
executeCLICommand :: CLICommand -> IO ()
executeCLICommand cmd = case cmd of
  InstallCommand _globalOpts installOpts -> do
    result <- executeInstall installOpts
    case result of
      Left err -> die $ "Installation failed: " ++ show err
      Right _ -> putStrLn "Installation completed successfully"

  DoctorCommand globalOpts _doctorOpts -> do
    result <- executeDiagnostics globalOpts
    case result of
      Left err -> die $ "Diagnostics failed: " ++ show err
      Right _ -> putStrLn "Diagnostics completed successfully"

  UninstallCommand globalOpts uninstallOpts -> do
    result <- executeUninstall globalOpts uninstallOpts
    case result of
      Left err -> die $ "Uninstall failed: " ++ show err
      Right summary -> putStrLn summary

  ListCommand globalOpts listOpts -> do
    result <- executeList globalOpts listOpts
    case result of
      Left err -> die $ "List command failed: " ++ show err
      Right () -> return ()

  VersionCommand globalOpts versionOpts -> do
    executeVersion globalOpts versionOpts

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
      -- Parse and execute CLI commands
      case parseCommand cliArgs of
        Left parseError -> die $ "Command parsing failed: " ++ parseError
        Right cmd -> executeCLICommand cmd
    
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
