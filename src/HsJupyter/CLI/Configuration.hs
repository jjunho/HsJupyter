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
    , doesDirectoryExist
    , getHomeDirectory
    , getCurrentDirectory
    , createDirectoryIfMissing
    , copyFile
    , removeFile
    )
import System.FilePath ((</>), takeDirectory, isAbsolute)
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

-- | Validate configuration and return normalized version (Phase 5 US3 enhanced)
validateConfiguration :: InstallationConfiguration -> IO (Either CLIDiagnostic InstallationConfiguration)
validateConfiguration config = do
    -- Validate installation scope
    scopeResult <- validateInstallScope (icInstallScope config)
    case scopeResult of
        Left err -> return $ Left err
        Right validatedScope -> do
            -- Validate custom paths if specified
            pathResult <- validateCustomPaths (icCustomPaths config)
            case pathResult of
                Left err -> return $ Left err
                Right validatedPaths -> do
                    -- Validate kernel configuration if specified
                    kernelResult <- validateKernelConfig (icKernelConfig config)
                    case kernelResult of
                        Left err -> return $ Left err
                        Right validatedKernelConfig -> do
                            let validatedConfig = config 
                                    { icInstallScope = validatedScope
                                    , icCustomPaths = validatedPaths
                                    , icKernelConfig = validatedKernelConfig
                                    }
                            return $ Right $ normalizeConfiguration validatedConfig

-- | Validate installation scope (Phase 5 US3)
validateInstallScope :: InstallScope -> IO (Either CLIDiagnostic InstallScope)
validateInstallScope SystemInstallation = do
    hasPermissions <- checkSystemInstallPermissions
    if hasPermissions
        then return $ Right SystemInstallation
        else return $ Left $ configurationError "Insufficient permissions for system install"
            "System-wide installation requires administrator privileges"
validateInstallScope UserInstallation = return $ Right UserInstallation
validateInstallScope AutoDetect = return $ Right AutoDetect
validateInstallScope (CustomPath path) = do
    validatedPath <- validateAndResolvePath path "custom install directory"
    case validatedPath of
        Left err -> return $ Left err
        Right resolvedPath -> return $ Right $ CustomPath resolvedPath

-- | Validate custom paths configuration (Phase 5 US3)
validateCustomPaths :: Maybe CustomPaths -> IO (Either CLIDiagnostic (Maybe CustomPaths))
validateCustomPaths Nothing = return $ Right Nothing
validateCustomPaths (Just paths) = do
    -- Validate Jupyter directory if specified
    jupyterDirResult <- case cpJupyterDir paths of
        Nothing -> return $ Right Nothing
        Just path -> do
            validated <- validateAndResolvePath path "Jupyter directory"
            case validated of
                Left err -> return $ Left err
                Right resolvedPath -> return $ Right $ Just resolvedPath
    
    case jupyterDirResult of
        Left err -> return $ Left err
        Right validatedJupyterDir -> do
            -- Validate kernelspec directory if specified
            kernelspecDirResult <- case cpKernelspecDir paths of
                Nothing -> return $ Right Nothing
                Just path -> do
                    validated <- validateAndResolvePath path "kernelspec directory"
                    case validated of
                        Left err -> return $ Left err
                        Right resolvedPath -> return $ Right $ Just resolvedPath
            
            case kernelspecDirResult of
                Left err -> return $ Left err
                Right validatedKernelspecDir -> do
                    -- Validate GHC path if specified  
                    ghcPathResult <- case cpGHCPath paths of
                        Nothing -> return $ Right Nothing
                        Just path -> do
                            validated <- validateExecutablePath path "GHC executable"
                            case validated of
                                Left err -> return $ Left err
                                Right resolvedPath -> return $ Right $ Just resolvedPath
                    
                    case ghcPathResult of
                        Left err -> return $ Left err
                        Right validatedGHCPath -> do
                            let validatedPaths = CustomPaths
                                    { cpJupyterDir = validatedJupyterDir
                                    , cpKernelspecDir = validatedKernelspecDir
                                    , cpGHCPath = validatedGHCPath
                                    }
                            return $ Right $ Just validatedPaths

-- | Validate kernel configuration (Phase 5 US3)
validateKernelConfig :: Maybe KernelConfig -> IO (Either CLIDiagnostic (Maybe KernelConfig))
validateKernelConfig Nothing = return $ Right Nothing
validateKernelConfig (Just kernelConfig) = do
    -- Validate resource limits
    let limits = kcResourceLimits kernelConfig
    limitsResult <- validateResourceLimits limits
    case limitsResult of
        Left err -> return $ Left err
        Right validatedLimits -> do
            let validatedConfig = kernelConfig { kcResourceLimits = validatedLimits }
            return $ Right $ Just validatedConfig

-- | Validate resource limits (Phase 5 US3)
validateResourceLimits :: ResourceLimits -> IO (Either CLIDiagnostic ResourceLimits)
validateResourceLimits limits = do
    -- Validate memory limit
    case rlMemoryLimitMB limits of
        Just memLimit | memLimit <= 0 -> return $ Left $ configurationError "Invalid memory limit"
            "Memory limit must be positive"
        Just memLimit | memLimit > 32768 -> return $ Left $ configurationError "Excessive memory limit"
            "Memory limit exceeds reasonable bounds (32GB)"
        _ -> return ()
    
    -- Validate timeout
    case rlTimeoutSeconds limits of
        Just timeout | timeout <= 0 -> return $ Left $ configurationError "Invalid timeout"
            "Timeout must be positive"
        Just timeout | timeout > 3600 -> return $ Left $ configurationError "Excessive timeout"
            "Timeout exceeds reasonable bounds (1 hour)"
        _ -> return ()
    
    -- Validate output size limit
    case rlMaxOutputSizeKB limits of
        Just outputLimit | outputLimit <= 0 -> return $ Left $ configurationError "Invalid output limit"
            "Output size limit must be positive"
        Just outputLimit | outputLimit > 1048576 -> return $ Left $ configurationError "Excessive output limit"
            "Output size limit exceeds reasonable bounds (1GB)"
        _ -> return ()
    
    return $ Right limits

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

-- | Validate and resolve a directory path (Phase 5 US3)
validateAndResolvePath :: FilePath -> String -> IO (Either CLIDiagnostic FilePath)
validateAndResolvePath path pathType = do
    -- Check if path is absolute or make it absolute
    absolutePath <- if isAbsolute path
        then return path
        else do
            currentDir <- getCurrentDirectory
            return $ currentDir </> path
    
    -- Check if directory exists or can be created
    exists <- doesDirectoryExist absolutePath
    if exists
        then do
            -- Check if directory is writable
            writable <- checkDirectoryWritable absolutePath
            if writable
                then return $ Right absolutePath
                else return $ Left $ configurationError ("Insufficient permissions for " <> T.pack pathType)
                    $ "Cannot write to directory: " <> absolutePath
        else do
            -- Try to create the directory
            result <- try $ createDirectoryIfMissing True absolutePath
            case result of
                Left (_ :: SomeException) -> return $ Left $ configurationError ("Cannot create " <> T.pack pathType)
                    $ "Failed to create directory: " <> absolutePath
                Right _ -> return $ Right absolutePath

-- | Validate executable path (Phase 5 US3)
validateExecutablePath :: FilePath -> String -> IO (Either CLIDiagnostic FilePath)
validateExecutablePath path executableType = do
    -- Make path absolute if needed
    absolutePath <- if isAbsolute path
        then return path
        else do
            currentDir <- getCurrentDirectory
            return $ currentDir </> path
    
    -- Check if file exists and is executable
    exists <- doesFileExist absolutePath
    if exists
        then do
            executable <- checkExecutablePermissions absolutePath
            if executable
                then return $ Right absolutePath
                else return $ Left $ configurationError ("Not executable: " <> T.pack executableType)
                    $ "File exists but is not executable: " <> absolutePath
        else return $ Left $ configurationError (T.pack executableType <> " not found")
            $ "File does not exist: " <> absolutePath

-- | Check if directory is writable
checkDirectoryWritable :: FilePath -> IO Bool
checkDirectoryWritable dir = do
    -- Try to create a temporary file to test write permissions
    let testFile = dir </> ".hsjupyter-write-test"
    result <- try $ do
        writeFile testFile "test"
        removeFile testFile
    case result of
        Left (_ :: SomeException) -> return False
        Right _ -> return True

-- | Check executable permissions (simplified for now)
checkExecutablePermissions :: FilePath -> IO Bool
checkExecutablePermissions _path = do
    -- In a real implementation, this would check file permissions
    -- For now, we assume if file exists, it might be executable
    return True

-- | Create a configuration error diagnostic
configurationError :: T.Text -> String -> CLIDiagnostic
configurationError title details = ConfigurationError (title <> ": " <> T.pack details)