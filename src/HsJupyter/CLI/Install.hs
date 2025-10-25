{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CLI Install module - handles kernel installation commands and operations
module HsJupyter.CLI.Install 
  ( executeInstall
  , detectJupyterEnvironment
  , validateJupyterEnvironment
  , validateKernelspecDirectories
  -- T016: Kernelspec directory discovery and validation
  , findKernelspecDirectories
  , ensureDirectoryExists
  , getKernelPath
  , validateKernelInstallation
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (filterM)
import Control.Exception (try, IOException)
import System.Directory 
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , getPermissions
  , writable
  , readable
  , getHomeDirectory
  )
import System.FilePath ((</>))
import System.Environment (lookupEnv)

import HsJupyter.CLI.Types 
  ( InstallScope(..)
  , CLIDiagnostic(..)
  , JupyterEnvironment(..)
  , PythonEnvironment(..)
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
        Right _validatedEnv -> do
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

-- | Filter kernelspec directories to only include accessible ones (T016: Enhanced implementation)
filterValidKernelspecDirs :: [FilePath] -> IO [FilePath]
filterValidKernelspecDirs dirs = filterAccessibleDirectories dirs

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
    CustomPath customDir -> do
      -- Validate the custom directory exists and is accessible
      validCustom <- filterAccessibleDirectories [customDir]
      if null validCustom
        then return $ Left $ ValidationError $ "Custom kernelspec directory not accessible: " <> T.pack customDir
        else return $ Right validCustom

-- | Filter directories to only include accessible ones (T016: Enhanced implementation)
filterAccessibleDirectories :: [FilePath] -> IO [FilePath]
filterAccessibleDirectories dirs = filterM isAccessibleDirectory dirs
  where
    isAccessibleDirectory dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then return False
        else do
          result <- try (getPermissions dir)
          case result of
            Left (_ :: IOException) -> return False
            Right perms -> return (readable perms && writable perms)

-- | Filter directories to user-accessible ones (T016: Enhanced implementation)
filterUserDirectories :: [FilePath] -> IO [FilePath]
filterUserDirectories dirs = do
  homeDir <- getHomeDirectory
  let userDirs = filter (isUserDirectory homeDir) dirs
  filterAccessibleDirectories userDirs
  where
    isUserDirectory homeDir dir = 
      T.pack homeDir `T.isInfixOf` T.pack dir || "/.local/" `T.isInfixOf` T.pack dir

-- | Filter directories to system ones (T016: Enhanced implementation)
filterSystemDirectories :: [FilePath] -> IO [FilePath]
filterSystemDirectories dirs = do
  let systemDirs = filter isSystemDirectory dirs
  filterAccessibleDirectories systemDirs
  where
    isSystemDirectory dir = 
      "/usr/" `T.isInfixOf` T.pack dir || 
      "/opt/" `T.isInfixOf` T.pack dir ||
      "/Library/" `T.isInfixOf` T.pack dir  -- macOS system directories-- | Validate Python environment compatibility with installation requirements
validatePythonCompatibility :: PythonEnvironment -> InstallOptions -> IO (Either CLIDiagnostic PythonEnvironment)
validatePythonCompatibility pythonEnv _options = do
  -- TODO: Implement actual Python version checking and GHC compatibility validation
  -- For T015, perform basic validation
  if pePath pythonEnv == ""
    then return $ Left $ ValidationError "Python executable not found"
    else return $ Right pythonEnv

-- ===========================================================================
-- T016: Kernelspec Directory Discovery and Validation Functions
-- ===========================================================================

-- | Find available kernelspec directories in the system (T016)
findKernelspecDirectories :: IO (Either CLIDiagnostic [FilePath])
findKernelspecDirectories = do
  -- Standard Jupyter kernelspec locations
  homeDir <- getHomeDirectory
  
  -- Get system and user kernelspec directories
  let standardDirs = 
        [ homeDir </> ".local" </> "share" </> "jupyter" </> "kernels"  -- User local
        , homeDir </> ".jupyter" </> "kernels"                          -- User jupyter
        , "/usr/local/share/jupyter/kernels"                           -- System local
        , "/usr/share/jupyter/kernels"                                 -- System global
        ]
  
  -- Check for conda environment directories
  condaEnvDirs <- findCondaKernelspecDirs
  
  -- Check for custom JUPYTER_PATH directories
  customDirs <- findCustomJupyterDirs
  
  -- Combine all potential directories
  let allDirs = standardDirs ++ condaEnvDirs ++ customDirs
  
  -- Filter to only existing and accessible directories
  validDirs <- filterAccessibleDirectories allDirs
  
  if null validDirs
    then return $ Left $ ValidationError "No accessible kernelspec directories found in system"
    else return $ Right validDirs

-- | Find conda environment kernelspec directories (T016)
findCondaKernelspecDirs :: IO [FilePath]
findCondaKernelspecDirs = do
  -- Check for conda environments
  condaEnv <- lookupEnv "CONDA_PREFIX"
  condaDefault <- lookupEnv "CONDA_DEFAULT_ENV"
  
  let condaDirs = case (condaEnv, condaDefault) of
        (Just prefix, _) -> 
          [ prefix </> "share" </> "jupyter" </> "kernels"
          ]
        (Nothing, Just defaultEnv) ->
          -- Try common conda installation paths
          [ "/opt/conda/envs" </> defaultEnv </> "share" </> "jupyter" </> "kernels"
          , "/usr/local/conda/envs" </> defaultEnv </> "share" </> "jupyter" </> "kernels"
          ]
        _ -> []
  
  -- Also check for miniconda/anaconda in home directory
  homeDir <- getHomeDirectory
  let homeConda = 
        [ homeDir </> "miniconda3" </> "share" </> "jupyter" </> "kernels"
        , homeDir </> "anaconda3" </> "share" </> "jupyter" </> "kernels"
        ]
  
  return $ condaDirs ++ homeConda

-- | Find custom Jupyter directories from JUPYTER_PATH environment variable (T016)
findCustomJupyterDirs :: IO [FilePath]
findCustomJupyterDirs = do
  jupyterPath <- lookupEnv "JUPYTER_PATH"
  case jupyterPath of
    Nothing -> return []
    Just paths -> do
      -- Split by colon on Unix systems (TODO: Handle Windows semicolon)
      let pathList = T.split (== ':') (T.pack paths)
      return $ map (</> "kernels") $ map T.unpack pathList

-- | Ensure directory exists, creating it if necessary (T016)
ensureDirectoryExists :: FilePath -> IO (Either CLIDiagnostic FilePath)
ensureDirectoryExists dir = do
  result <- try $ createDirectoryIfMissing True dir
  case result of
    Left (ex :: IOException) -> 
      return $ Left $ ValidationError $ "Failed to create directory " <> T.pack dir <> ": " <> T.pack (show ex)
    Right () -> do
      -- Verify the directory was created and is accessible
      exists <- doesDirectoryExist dir
      if exists
        then do
          perms <- getPermissions dir
          if writable perms
            then return $ Right dir
            else return $ Left $ ValidationError $ "Directory " <> T.pack dir <> " is not writable"
        else return $ Left $ ValidationError $ "Failed to create directory " <> T.pack dir

-- | Get the full path to kernel.json for a given kernelspec directory and kernel name (T016)
getKernelPath :: FilePath -> Text -> FilePath
getKernelPath kernelspecDir kernelName = 
  kernelspecDir </> T.unpack kernelName </> "kernel.json"

-- | Validate that a kernelspec directory can accommodate a new kernel installation (T016)
validateKernelInstallation :: FilePath -> Text -> IO (Either CLIDiagnostic FilePath)
validateKernelInstallation kernelspecDir kernelName = do
  let kernelDir = kernelspecDir </> T.unpack kernelName
  let kernelPath = getKernelPath kernelspecDir kernelName
  
  -- Ensure the parent kernelspec directory exists
  kernelspecExists <- doesDirectoryExist kernelspecDir
  if not kernelspecExists
    then return $ Left $ ValidationError $ "Kernelspec directory does not exist: " <> T.pack kernelspecDir
    else do
      -- Check if kernel directory already exists
      kernelDirExists <- doesDirectoryExist kernelDir
      if kernelDirExists
        then return $ Left $ ValidationError $ "Kernel already exists at: " <> T.pack kernelDir
        else do
          -- Try to create the kernel directory
          ensureResult <- ensureDirectoryExists kernelDir
          case ensureResult of
            Left diag -> return $ Left diag
            Right _createdDir -> return $ Right kernelPath