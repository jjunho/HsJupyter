{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : HsJupyter.CLI.Utilities
Description : System detection and validation utilities for CLI operations
Copyright   : (c) HsJupyter Contributors 2024
License     : MIT
Maintainer  : dev@hsjupyter.org
Stability   : experimental

This module provides utilities for detecting system environment and validating
installation prerequisites according to the constitutional framework.
-}

module HsJupyter.CLI.Utilities
    ( -- * System Detection
      detectJupyterEnvironment
    , detectSystemPython
    , validateJupyterInstallation
    
      -- * Path Resolution
    , resolveJupyterDataDir
    , resolveJupyterConfigDir
    , resolveJupyterRuntimeDir
    , resolveKernelspecDir
    , ensureDirectoryExists
    
      -- * Validation Utilities
    , validateSystemRequirements
    , checkWritePermissions
    , detectConflictingInstallations
    
      -- * Error Handling
    , systemError
    , validationError
    ) where

import qualified Data.Text as T
import Data.Aeson (object)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory 
    ( doesDirectoryExist
    , doesFileExist
    , createDirectoryIfMissing
    , getPermissions
    , writable
    , getHomeDirectory
    )
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcessWithExitCode)

import HsJupyter.CLI.Types

-- | Detect the current Jupyter environment configuration
detectJupyterEnvironment :: MonadIO m => m (Either CLIDiagnostic JupyterEnvironment)
detectJupyterEnvironment = liftIO $ do
    pythonPath <- detectSystemPython
    case pythonPath of
        Left diag -> return $ Left diag
        Right python -> do
            jupyterResult <- checkJupyterInstallation python
            case jupyterResult of
                Left diag -> return $ Left diag
                Right _jupyterPath -> do
                    dataDir <- resolveJupyterDataDir
                    return $ Right JupyterEnvironment
                        { jeKernelspecDirs = [dataDir </> "kernels"]
                        , jePythonEnv = PythonEnvironment 
                            { pePath = python
                            , peVersion = "3.x"  -- We'll get actual version later
                            , peEnvironment = Nothing
                            }
                        , jeVersion = JupyterVersion
                            { jvLab = Nothing
                            , jvNotebook = Nothing  
                            , jvCore = "unknown"
                            }
                        , jeInstallType = UserLocal
                        }

-- | Detect system Python executable
detectSystemPython :: MonadIO m => m (Either CLIDiagnostic FilePath)
detectSystemPython = liftIO $ do
    -- Try common Python executable names
    let pythonCandidates = ["python3", "python"]
    result <- tryPythonCandidates pythonCandidates
    case result of
        Nothing -> return $ Left $ systemError "No Python executable found" 
            "Please ensure Python 3.x is installed and available in PATH"
        Just pythonPath -> do
            -- Validate Python version
            versionResult <- checkPythonVersion pythonPath
            case versionResult of
                Left diag -> return $ Left diag
                Right _ -> return $ Right pythonPath

-- | Try to find a working Python executable from candidates
tryPythonCandidates :: [String] -> IO (Maybe FilePath)
tryPythonCandidates [] = return Nothing
tryPythonCandidates (candidate:rest) = do
    result <- try $ readProcessWithExitCode candidate ["--version"] ""
    case result of
        Left (_ :: SomeException) -> tryPythonCandidates rest
        Right (ExitSuccess, _, _) -> return $ Just candidate
        Right (ExitFailure _, _, _) -> tryPythonCandidates rest

-- | Check if Python version meets requirements (3.7+)
checkPythonVersion :: FilePath -> IO (Either CLIDiagnostic ())
checkPythonVersion pythonPath = do
    result <- try $ readProcessWithExitCode pythonPath ["-c", "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')"] ""
    case result of
        Left (_ :: SomeException) -> return $ Left $ systemError "Failed to check Python version"
            "Could not execute Python version check"
        Right (ExitSuccess, versionStr, _) -> do
            let version = T.strip $ T.pack versionStr
            if isPythonVersionSupported version
                then return $ Right ()
                else return $ Left $ systemError "Unsupported Python version"
                    $ "Found Python " <> T.unpack version <> ", but Python 3.7+ is required"
        Right (ExitFailure _, _, stderr) -> return $ Left $ systemError "Python version check failed"
            $ "Error: " <> stderr

-- | Check if Python version string represents a supported version
isPythonVersionSupported :: T.Text -> Bool
isPythonVersionSupported versionStr = 
    case T.splitOn "." versionStr of
        [majorStr, minorStr] -> 
            case (readMaybe $ T.unpack majorStr :: Maybe Int, readMaybe $ T.unpack minorStr :: Maybe Int) of
                (Just major, Just minor) -> major >= 3 && (major > 3 || minor >= 7)
                _ -> False
        _ -> False
  where
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

-- | Check if Jupyter is installed and accessible
checkJupyterInstallation :: FilePath -> IO (Either CLIDiagnostic FilePath)
checkJupyterInstallation pythonPath = do
    result <- try $ readProcessWithExitCode pythonPath ["-m", "jupyter", "--version"] ""
    case result of
        Left (_ :: SomeException) -> return $ Left $ systemError "Jupyter not found"
            "Please install Jupyter: pip install jupyter"
        Right (ExitSuccess, _, _) -> do
            -- Get jupyter executable path
            jupyterResult <- try $ readProcessWithExitCode pythonPath ["-c", "import jupyter; print(jupyter.__file__)"] ""
            case jupyterResult of
                Left (_ :: SomeException) -> return $ Left $ systemError "Cannot locate Jupyter installation"
                    "Jupyter appears to be installed but cannot be located"
                Right (ExitSuccess, jupyterPath, _) -> return $ Right $ takeDirectory $ T.unpack $ T.strip $ T.pack jupyterPath
                Right (ExitFailure _, _, stderr) -> return $ Left $ systemError "Jupyter location failed"
                    $ "Error: " <> stderr
        Right (ExitFailure _, _, stderr) -> return $ Left $ systemError "Jupyter check failed"
            $ "Error: " <> stderr

-- | Validate that Jupyter installation meets requirements
validateJupyterInstallation :: JupyterEnvironment -> IO (Either CLIDiagnostic ())
validateJupyterInstallation env = do
    -- Check if jupyter kernelspec command is available
    let pythonPath = pePath (jePythonEnv env)
    result <- try $ readProcessWithExitCode pythonPath ["-m", "jupyter", "kernelspec", "list"] ""
    case result of
        Left (_ :: SomeException) -> return $ Left $ systemError "Jupyter kernelspec not available"
            "Jupyter installation appears incomplete - kernelspec command not found"
        Right (ExitSuccess, _, _) -> return $ Right ()
        Right (ExitFailure _, _, stderr) -> return $ Left $ systemError "Jupyter kernelspec failed"
            $ "Error: " <> stderr

-- | Resolve Jupyter data directory
resolveJupyterDataDir :: IO FilePath
resolveJupyterDataDir = do
    maybeDataDir <- lookupEnv "JUPYTER_DATA_DIR"
    case maybeDataDir of
        Just dataDir -> return dataDir
        Nothing -> do
            home <- getHomeDirectory
            return $ home </> ".local" </> "share" </> "jupyter"

-- | Resolve Jupyter config directory
resolveJupyterConfigDir :: IO FilePath
resolveJupyterConfigDir = do
    maybeConfigDir <- lookupEnv "JUPYTER_CONFIG_DIR"
    case maybeConfigDir of
        Just configDir -> return configDir
        Nothing -> do
            home <- getHomeDirectory
            return $ home </> ".jupyter"

-- | Resolve Jupyter runtime directory
resolveJupyterRuntimeDir :: IO FilePath
resolveJupyterRuntimeDir = do
    maybeRuntimeDir <- lookupEnv "JUPYTER_RUNTIME_DIR"
    case maybeRuntimeDir of
        Just runtimeDir -> return runtimeDir
        Nothing -> do
            home <- getHomeDirectory
            return $ home </> ".local" </> "share" </> "jupyter" </> "runtime"

-- | Resolve kernelspec directory for the given kernel name
resolveKernelspecDir :: JupyterEnvironment -> T.Text -> FilePath
resolveKernelspecDir env kernelName = 
    case jeKernelspecDirs env of
        [] -> "/tmp" </> "kernels" </> T.unpack kernelName  -- Fallback
        (dir:_) -> dir </> T.unpack kernelName

-- | Ensure directory exists, creating it if necessary
ensureDirectoryExists :: FilePath -> IO (Either CLIDiagnostic ())
ensureDirectoryExists dirPath = do
    result <- try $ createDirectoryIfMissing True dirPath
    case result of
        Left (_ :: SomeException) -> return $ Left $ systemError "Failed to create directory"
            $ "Could not create directory: " <> dirPath
        Right _ -> return $ Right ()

-- | Validate system requirements for installation
validateSystemRequirements :: IO (Either CLIDiagnostic ())
validateSystemRequirements = do
    pythonResult <- detectSystemPython
    case pythonResult of
        Left diag -> return $ Left diag
        Right python -> do
            jupyterResult <- checkJupyterInstallation python
            case jupyterResult of
                Left diag -> return $ Left diag
                Right _ -> return $ Right ()

-- | Check write permissions for a directory
checkWritePermissions :: FilePath -> IO (Either CLIDiagnostic ())
checkWritePermissions dirPath = do
    exists <- doesDirectoryExist dirPath
    if not exists
        then do
            result <- ensureDirectoryExists dirPath
            case result of
                Left diag -> return $ Left diag
                Right _ -> do
                    permissions <- getPermissions dirPath
                    if writable permissions
                        then return $ Right ()
                        else return $ Left $ systemError "Insufficient permissions"
                            $ "No write access to directory: " <> dirPath
        else do
            permissions <- getPermissions dirPath
            if writable permissions
                then return $ Right ()
                else return $ Left $ systemError "Insufficient permissions"
                    $ "No write access to directory: " <> dirPath

-- | Detect conflicting kernel installations
detectConflictingInstallations :: JupyterEnvironment -> T.Text -> IO [KernelInstallation]
detectConflictingInstallations env kernelName = do
    let kernelDir = resolveKernelspecDir env kernelName
    exists <- doesDirectoryExist kernelDir
    if exists
        then do
            let kernelJsonFile = kernelDir </> "kernel.json"
            jsonExists <- doesFileExist kernelJsonFile
            if jsonExists
                then return [KernelInstallation
                    { kiKernelspecPath = kernelJsonFile
                    , kiDisplayName = kernelName
                    , kiVersion = "unknown"
                    , kiGHCPath = "ghc"  -- Default, will be detected properly later
                    , kiStatus = Installed
                    , kiConfiguration = KernelConfig
                        { kcResourceLimits = ResourceLimits Nothing Nothing Nothing
                        , kcDisplayName = kernelName
                        , kcLanguage = "haskell"
                        , kcInterruptMode = Signal
                        , kcMetadata = object []
                        }
                    }]
                else return []
        else return []

-- | Create a system error diagnostic
systemError :: T.Text -> String -> CLIDiagnostic
systemError title details = SystemIntegrationError (title <> ": " <> T.pack details)

-- | Create a validation error diagnostic
validationError :: T.Text -> String -> CLIDiagnostic
validationError title details = ValidationError (title <> ": " <> T.pack details)