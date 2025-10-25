{-# LANGUAGE OverloadedStrings #-}

-- | CLI Install module - handles kernel installation commands and operations
module HsJupyter.CLI.Install 
  ( executeInstall
  , detectJupyterEnvironment
  , validateJupyterEnvironment
  , validateKernelspecDirectories
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import HsJupyter.CLI.Types 
  ( InstallScope(..)
  , ValidationLevel(..)
  , CLIDiagnostic(..)
  , JupyterEnvironment(..)
  , PythonEnvironment(..)
  , JupyterVersion(..)
  , InstallationType(..)
  )
import HsJupyter.CLI.Commands (InstallOptions(..))
import qualified HsJupyter.CLI.Utilities as Utilities

-- | Execute kernel installation with given options (T015: Jupyter environment detection)
executeInstall :: InstallOptions -> IO (Either CLIDiagnostic ())
executeInstall options = do
  -- Step 1: Detect Jupyter environment (T015 implementation)
  jupyterEnvResult <- detectJupyterEnvironment
  case jupyterEnvResult of
    Left diag -> return $ Left diag
    Right jupyterEnv -> do
      -- Step 2: Validate environment meets installation requirements  
      validationResult <- validateJupyterEnvironment jupyterEnv options
      case validationResult of
        Left diag -> return $ Left diag
        Right validatedEnv -> do
          -- TODO: Continue with actual installation steps (T016-T018)
          -- For now, return success to indicate environment detection works
          return $ Right ()

-- | Detect current Jupyter environment using system utilities (T015)
detectJupyterEnvironment :: IO (Either CLIDiagnostic JupyterEnvironment)
detectJupyterEnvironment = do
  -- Use the Utilities module function for core detection
  result <- Utilities.detectJupyterEnvironment
  case result of
    Left diag -> return $ Left diag
    Right jupyterEnv -> do
      -- Enhanced detection with additional validation
      enhancedResult <- enhanceJupyterEnvironment jupyterEnv
      return $ Right enhancedResult

-- | Enhance detected Jupyter environment with additional validation
enhanceJupyterEnvironment :: JupyterEnvironment -> IO JupyterEnvironment
enhanceJupyterEnvironment jupyterEnv = do
  -- Verify kernelspec directories are accessible
  validatedDirs <- filterValidKernelspecDirs (jeKernelspecDirs jupyterEnv)
  
  -- Detect more precise Python version information
  enhancedPython <- enhancePythonEnvironment (jePythonEnv jupyterEnv)
  
  return jupyterEnv 
    { jeKernelspecDirs = validatedDirs
    , jePythonEnv = enhancedPython
    }

-- | Filter kernelspec directories to only include accessible ones
filterValidKernelspecDirs :: [FilePath] -> IO [FilePath]
filterValidKernelspecDirs dirs = do
  -- TODO: Implement actual directory validation (will be enhanced in T016)
  -- For now, return all directories (basic implementation for T015)
  return dirs

-- | Enhance Python environment information with more details
enhancePythonEnvironment :: PythonEnvironment -> IO PythonEnvironment
enhancePythonEnvironment pythonEnv = do
  -- TODO: Get actual Python version information
  -- For now, return enhanced basic info (will be improved in subsequent tasks)
  return pythonEnv
    { peVersion = "3.x.x" -- Placeholder - will be detected properly
    }

-- | Validate detected Jupyter environment against install options (T015)
validateJupyterEnvironment :: JupyterEnvironment -> InstallOptions -> IO (Either CLIDiagnostic JupyterEnvironment)
validateJupyterEnvironment jupyterEnv options = do
  -- Validate kernelspec directories are accessible for the chosen scope
  kernelspecValidation <- validateKernelspecDirectories (jeKernelspecDirs jupyterEnv) options
  case kernelspecValidation of
    Left diag -> return $ Left diag
    Right validatedDirs -> do
      -- Update environment with validated directories
      let updatedEnv = jupyterEnv { jeKernelspecDirs = validatedDirs }
      
      -- Validate Python environment compatibility
      pythonValidation <- validatePythonCompatibility (jePythonEnv jupyterEnv) options
      case pythonValidation of
        Left diag -> return $ Left diag
        Right validatedPython -> do
          let finalEnv = updatedEnv { jePythonEnv = validatedPython }
          return $ Right finalEnv

-- | Validate kernelspec directories exist and are writable for installation scope (T015)
validateKernelspecDirectories :: [FilePath] -> InstallOptions -> IO (Either CLIDiagnostic [FilePath])
validateKernelspecDirectories dirs options = do
  -- Filter directories based on installation scope and accessibility
  case ioScope options of
    AutoDetect -> do
      -- Try to find the best available directory
      validDirs <- filterAccessibleDirectories dirs
      if null validDirs
        then return $ Left $ ValidationError "No accessible kernelspec directories found"
        else return $ Right validDirs
    UserInstallation -> do
      -- Filter to user-accessible directories only
      userDirs <- filterUserDirectories dirs
      if null userDirs
        then return $ Left $ ValidationError "No user-accessible kernelspec directories found"
        else return $ Right userDirs
    SystemInstallation -> do
      -- Filter to system directories only
      systemDirs <- filterSystemDirectories dirs
      if null systemDirs
        then return $ Left $ ValidationError "No system kernelspec directories found or insufficient permissions"
        else return $ Right systemDirs

-- | Filter directories to only include accessible ones
filterAccessibleDirectories :: [FilePath] -> IO [FilePath]
filterAccessibleDirectories dirs = do
  -- TODO: Implement actual accessibility checking (enhanced in T016)
  -- For T015, return all directories as accessible (basic implementation)
  return dirs

-- | Filter directories to user-accessible ones
filterUserDirectories :: [FilePath] -> IO [FilePath]
filterUserDirectories dirs = do
  -- TODO: Implement actual user directory filtering (enhanced in T016)
  -- For T015, return directories that look like user directories
  return $ filter isUserDirectory dirs
  where
    isUserDirectory dir = "/home/" `T.isInfixOf` T.pack dir || "/.local/" `T.isInfixOf` T.pack dir

-- | Filter directories to system ones
filterSystemDirectories :: [FilePath] -> IO [FilePath]
filterSystemDirectories dirs = do
  -- TODO: Implement actual system directory filtering (enhanced in T016)
  -- For T015, return directories that look like system directories
  return $ filter isSystemDirectory dirs
  where
    isSystemDirectory dir = "/usr/" `T.isInfixOf` T.pack dir || "/opt/" `T.isInfixOf` T.pack dir

-- | Validate Python environment compatibility with installation requirements
validatePythonCompatibility :: PythonEnvironment -> InstallOptions -> IO (Either CLIDiagnostic PythonEnvironment)
validatePythonCompatibility pythonEnv _options = do
  -- TODO: Implement actual Python version checking and GHC compatibility validation
  -- For T015, perform basic validation
  if pePath pythonEnv == ""
    then return $ Left $ ValidationError "Python executable not found"
    else return $ Right pythonEnv