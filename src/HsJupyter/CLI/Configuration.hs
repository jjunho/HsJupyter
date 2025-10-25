{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : HsJupyter.CLI.Configuration
Description : Configuration management for CLI operations
Copyright   : (c) HsJupyter Contributors 2024
License     : MIT
Maintainer  : dev@hsjupyter.org
Stability   : experimental

This module provides configuration management utilities for CLI operations,
including reading, writing, and validating configuration files.
-}

module HsJupyter.CLI.Configuration
    ( -- * Configuration Management
      loadConfiguration
    , saveConfiguration
    , getDefaultConfiguration
    
      -- * Configuration Validation
    , validateConfiguration
    , normalizeConfiguration
    
      -- * Configuration File Operations
    , findConfigurationFile
    , createDefaultConfigFile
    , backupConfigFile
    
      -- * Utilities
    , mergeConfigurations
    , configurationError
    ) where

import qualified Data.Text as T
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory 
    ( doesFileExist
    , getHomeDirectory
    , createDirectoryIfMissing
    , copyFile
    )
import System.FilePath ((</>), takeDirectory)
import System.Environment (lookupEnv)

import HsJupyter.CLI.Types

-- | Load configuration from file or return default
loadConfiguration :: MonadIO m => Maybe FilePath -> m (Either CLIDiagnostic InstallationConfiguration)
loadConfiguration maybeConfigPath = liftIO $ do
    configPath <- case maybeConfigPath of
        Just path -> return path
        Nothing -> findConfigurationFile
    
    exists <- doesFileExist configPath
    if exists
        then do
            result <- try $ eitherDecodeFileStrict configPath
            case result of
                Left (_ :: SomeException) -> return $ Left $ configurationError "Failed to read configuration file"
                    $ "Could not read configuration from: " <> configPath
                Right (Left parseErr) -> return $ Left $ configurationError "Invalid configuration format"
                    $ "Parse error: " <> parseErr
                Right (Right config) -> do
                    validationResult <- validateConfiguration config
                    case validationResult of
                        Left diag -> return $ Left diag
                        Right validConfig -> return $ Right validConfig
        else do
            -- Return default configuration if no file exists
            return $ Right getDefaultConfiguration

-- | Save configuration to file
saveConfiguration :: MonadIO m => FilePath -> InstallationConfiguration -> m (Either CLIDiagnostic ())
saveConfiguration configPath config = liftIO $ do
    -- Ensure directory exists
    let configDir = takeDirectory configPath
    createDirectoryIfMissing True configDir
    
    -- Backup existing file if it exists, then save
    exists <- doesFileExist configPath
    if exists
        then do
            backupResult <- backupConfigFile configPath
            case backupResult of
                Left diag -> return $ Left diag
                Right _ -> do
                    -- Continue with saving
                    result <- try $ encodeFile configPath config
                    case result of
                        Left (_ :: SomeException) -> return $ Left $ configurationError "Failed to save configuration"
                            $ "Could not write configuration to: " <> configPath
                        Right _ -> return $ Right ()
        else do
            -- Save directly if file doesn't exist
            result <- try $ encodeFile configPath config
            case result of
                Left (_ :: SomeException) -> return $ Left $ configurationError "Failed to save configuration"
                    $ "Could not write configuration to: " <> configPath
                Right _ -> return $ Right ()

-- | Get default configuration
getDefaultConfiguration :: InstallationConfiguration
getDefaultConfiguration = InstallationConfiguration
    { icInstallScope = UserInstallation
    , icCustomPaths = Nothing
    , icKernelConfig = Nothing
    , icForceReinstall = False
    , icQuietMode = False
    , icValidationLevel = BasicValidation
    }

-- | Validate configuration and return normalized version
validateConfiguration :: InstallationConfiguration -> IO (Either CLIDiagnostic InstallationConfiguration)
validateConfiguration config = do
    -- Validate installation scope
    case icInstallScope config of
        SystemInstallation -> do
            -- Check if we have system-level permissions
            hasPermissions <- checkSystemInstallPermissions
            if hasPermissions
                then return $ Right $ normalizeConfiguration config
                else return $ Left $ configurationError "Insufficient permissions for system install"
                    "System-wide installation requires administrator privileges"
        UserInstallation -> return $ Right $ normalizeConfiguration config
        AutoDetect -> return $ Right $ normalizeConfiguration config
        CustomPath _path -> return $ Right $ normalizeConfiguration config

-- | Normalize configuration (apply defaults, resolve paths, etc.)
normalizeConfiguration :: InstallationConfiguration -> InstallationConfiguration
normalizeConfiguration config = config
    { icValidationLevel = if icQuietMode config then NoValidation else icValidationLevel config
    }

-- | Find configuration file in standard locations
findConfigurationFile :: IO FilePath
findConfigurationFile = do
    -- Check environment variable first
    maybeConfigPath <- lookupEnv "HS_JUPYTER_CONFIG"
    case maybeConfigPath of
        Just path -> return path
        Nothing -> do
            home <- getHomeDirectory
            return $ home </> ".hsjupyter" </> "config.json"

-- | Create default configuration file
createDefaultConfigFile :: MonadIO m => FilePath -> m (Either CLIDiagnostic ())
createDefaultConfigFile configPath = liftIO $ do
    let defaultConfig = getDefaultConfiguration
    saveConfiguration configPath defaultConfig

-- | Backup existing configuration file
backupConfigFile :: FilePath -> IO (Either CLIDiagnostic ())
backupConfigFile configPath = do
    let backupPath = configPath <> ".backup"
    result <- try $ copyFile configPath backupPath
    case result of
        Left (_ :: SomeException) -> return $ Left $ configurationError "Failed to backup configuration"
            $ "Could not create backup at: " <> backupPath
        Right _ -> return $ Right ()

-- | Merge two configurations (second overrides first)
mergeConfigurations :: InstallationConfiguration -> InstallationConfiguration -> InstallationConfiguration
mergeConfigurations base override = InstallationConfiguration
    { icInstallScope = icInstallScope override
    , icCustomPaths = icCustomPaths override `orElse` icCustomPaths base
    , icKernelConfig = icKernelConfig override `orElse` icKernelConfig base
    , icForceReinstall = icForceReinstall override || icForceReinstall base
    , icQuietMode = icQuietMode override || icQuietMode base
    , icValidationLevel = icValidationLevel override
    }
  where
    orElse :: Maybe a -> Maybe a -> Maybe a
    Just a `orElse` _ = Just a
    Nothing `orElse` b = b

-- | Check if system-level installation permissions are available
checkSystemInstallPermissions :: IO Bool
checkSystemInstallPermissions = do
    -- For now, just check if we're not root (which would be dangerous anyway)
    -- Real implementation would check specific directory permissions
    return False  -- Default to user install for safety

-- | Create a configuration error diagnostic
configurationError :: T.Text -> String -> CLIDiagnostic
configurationError title details = ConfigurationError (title <> ": " <> T.pack details)