{-# LANGUAGE OverloadedStrings #-}

-- | CLI Install module - handles kernel installation commands and operations
module HsJupyter.CLI.Install 
  ( InstallCommand(..)
  , InstallOptions(..)
  , defaultInstallOptions
  , parseInstallCommand
  , validateInstallOptions
  , executeInstall
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)

import HsJupyter.CLI.Types 
  ( InstallScope(..)
  , ValidationLevel(..)
  , CLIDiagnostic(..)
  )

-- | Represents an install command with parsed options
data InstallCommand = InstallCommand InstallOptions
  deriving (Show, Eq)

-- | Installation options parsed from command line
data InstallOptions = InstallOptions
  { ioScope            :: InstallScope     -- ^ Installation scope (user/system/auto)
  , ioForceReinstall   :: Bool             -- ^ Force overwrite existing installation
  , ioValidationLevel  :: ValidationLevel -- ^ Post-install validation depth
  , ioDisplayName      :: Maybe Text       -- ^ Custom kernel display name
  , ioGHCPath          :: Maybe FilePath   -- ^ Custom GHC executable path
  , ioJupyterDir       :: Maybe FilePath   -- ^ Custom Jupyter directory
  , ioKernelspecDir    :: Maybe FilePath   -- ^ Custom kernelspec directory
  , ioQuietMode        :: Bool             -- ^ Suppress interactive prompts
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
  , ioQuietMode = False               -- Interactive by default
  }

-- | Parse install command from command line arguments
parseInstallCommand :: [String] -> Either String InstallCommand
parseInstallCommand args = do
  options <- parseInstallArgs args defaultInstallOptions
  return $ InstallCommand options

-- | Parse install command arguments recursively
parseInstallArgs :: [String] -> InstallOptions -> Either String InstallOptions
parseInstallArgs [] options = Right options
parseInstallArgs ("install":rest) options = parseInstallArgs rest options
parseInstallArgs ("--user":rest) options = 
  parseInstallArgs rest options { ioScope = UserInstallation }
parseInstallArgs ("--system":rest) options = 
  parseInstallArgs rest options { ioScope = SystemInstallation }
parseInstallArgs ("--force":rest) options = 
  parseInstallArgs rest options { ioForceReinstall = True }
parseInstallArgs ("--quiet":rest) options = 
  parseInstallArgs rest options { ioQuietMode = True }
parseInstallArgs ("--display-name":name:rest) options = 
  parseInstallArgs rest options { ioDisplayName = Just (T.pack name) }
parseInstallArgs ("--ghc-path":path:rest) options = 
  parseInstallArgs rest options { ioGHCPath = Just path }
parseInstallArgs ("--jupyter-dir":path:rest) options = 
  parseInstallArgs rest options { ioJupyterDir = Just path }
parseInstallArgs ("--kernelspec-dir":path:rest) options = 
  parseInstallArgs rest options { ioKernelspecDir = Just path }
parseInstallArgs ("--validation":level:rest) options = do
  validationLevel <- parseValidationLevel level
  parseInstallArgs rest options { ioValidationLevel = validationLevel }
parseInstallArgs (arg:_) _ = 
  Left $ "Unknown install option: " ++ arg

-- | Parse validation level from string
parseValidationLevel :: String -> Either String ValidationLevel
parseValidationLevel "none" = Right NoValidation
parseValidationLevel "basic" = Right BasicValidation  
parseValidationLevel "full" = Right FullValidation
parseValidationLevel invalid = Left $ "Invalid validation level: " ++ invalid

-- | Validate install options for consistency and feasibility
validateInstallOptions :: InstallOptions -> Either String InstallOptions
validateInstallOptions options = do
  -- Validate custom GHC path exists if specified
  case ioGHCPath options of
    Nothing -> return ()
    Just ghcPath -> do
      exists <- pure False -- TODO: implement doesFileExist check
      when (not exists) $ 
        Left $ "Custom GHC path does not exist: " ++ ghcPath
  
  -- Validate custom directories exist if specified
  case ioJupyterDir options of
    Nothing -> return ()
    Just _jupyterDir -> do
      -- TODO: implement directory existence check
      return ()
  
  case ioKernelspecDir options of
    Nothing -> return ()
    Just _kernelspecDir -> do
      -- TODO: implement directory existence check
      return ()
  
  return options

-- | Execute kernel installation with given options
executeInstall :: InstallOptions -> IO (Either CLIDiagnostic ())
executeInstall _options = do
  -- TODO: Implement actual installation logic
  -- This is a placeholder that will be expanded in subsequent tasks
  return $ Right ()