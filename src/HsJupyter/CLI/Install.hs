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
  -- T017: Kernel.json generation with constitutional compliance
  , generateKernelJson
  , installKernelJson
  , writeKernelJson
  , validateKernelJson
  -- T018: Kernel registration and file system operations
  , executeKernelRegistration
  , selectInstallationDirectory
  , resolveKernelName
  , resolveGHCPath
  , verifyKernelInstallation
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (filterM, when)
import Control.Exception (try, IOException, SomeException, catch, finally)
import qualified Control.Exception
import qualified System.Timeout
import System.Directory 
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getPermissions
  , writable
  , readable
  , getHomeDirectory
  , findExecutable
  , removeDirectoryRecursive
  )
import System.FilePath ((</>), takeDirectory)
import System.Environment (lookupEnv, getExecutablePath)
import Data.Aeson 
  ( Value(..)
  , Object
  , Array
  , (.=)
  , object
  , encode
  , eitherDecode
  )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)

-- T019: ResourceGuard and ErrorHandling integration for constitutional compliance
import HsJupyter.Runtime.ResourceGuard 
  ( ResourceGuard
  , ResourceLimits(..)  
  , withResourceGuard
  , defaultResourceLimits
  , ResourceViolation(..)
  )
import HsJupyter.Runtime.ErrorHandling
  ( withErrorContext
  , enrichDiagnostic
  )

import HsJupyter.CLI.Types 
  ( InstallScope(..)
  , CLIDiagnostic(..)
  , JupyterEnvironment(..)
  , PythonEnvironment(..)
  )
import HsJupyter.CLI.Commands (InstallOptions(..))
import qualified HsJupyter.CLI.Utilities as Utilities

-- ===========================================================================
-- T019: CLI-specific ResourceGuard helper functions
-- ===========================================================================

-- | CLI-specific resource error handling that works with CLIDiagnostic
withCLIResourceError :: Text -> IO (Either CLIDiagnostic a) -> IO (Either CLIDiagnostic a)
withCLIResourceError operation action = do
  action `catch` \(violation :: ResourceViolation) ->
    return $ Left (SystemIntegrationError $ "Resource violation during " <> operation <> ": " <> T.pack (show violation))

-- | CLI-specific resource cleanup pattern
withCLIResourceCleanup :: IO () -> IO (Either CLIDiagnostic a) -> IO (Either CLIDiagnostic a)
withCLIResourceCleanup cleanup action = 
  action `finally` cleanup
  where
    finally :: IO a -> IO b -> IO a
    finally = Control.Exception.finally

-- | CLI-specific timeout wrapper for operations
withCLITimeout :: Int -> Text -> IO (Either CLIDiagnostic a) -> IO (Either CLIDiagnostic a)
withCLITimeout timeoutSeconds operation action = do
  result <- timeout (timeoutSeconds * 1000000) action
  case result of
    Nothing -> return $ Left (SystemIntegrationError $ "Operation timed out after " <> T.pack (show timeoutSeconds) <> " seconds: " <> operation)
    Just r -> return r
  where
    timeout :: Int -> IO a -> IO (Maybe a)
    timeout = System.Timeout.timeout

-- | Execute kernel installation with given options (T015: Jupyter environment detection, T018: Complete installation)
-- T019: Enhanced with ResourceGuard integration for constitutional compliance
executeInstall :: InstallOptions -> IO (Either CLIDiagnostic ())
executeInstall options = withErrorContext "kernel-installation" $ do
  -- Constitutional resource limits for installation operations (<2min, <100MB)
  let installationLimits = defaultResourceLimits
        { rcMaxCpuSeconds = 120.0  -- 2 minute timeout for installation
        , rcMaxMemoryMB = 100      -- 100MB memory limit per specification
        , rcMaxOutputBytes = 10485760  -- 10MB output limit for logs
        }
  
  result <- withResourceGuard installationLimits $ \guard -> do
    -- Step 1: Detect Jupyter environment (T015 implementation)
    jupyterEnvResult <- withCLIResourceError "jupyter-environment-detection" detectJupyterEnvironment
    case jupyterEnvResult of
      Left diag -> return $ Left diag
      Right jupyterEnv -> do
        -- Step 2: Validate environment meets installation requirements  
        validationResult <- withCLIResourceError "jupyter-environment-validation" $ 
          validateJupyterEnvironment jupyterEnv options
        case validationResult of
          Left diag -> return $ Left diag
          Right validatedEnv -> do
            -- Step 3: Execute complete kernel registration workflow (T018)
            registrationResult <- withCLIResourceError "kernel-registration" $ 
              executeKernelRegistration options validatedEnv
            case registrationResult of
              Left diag -> return $ Left diag
              Right _ -> return $ Right ()
  
  return result

-- | Detect current Jupyter environment using system utilities (T015)
-- T019: Enhanced with ResourceGuard protection for system detection operations
detectJupyterEnvironment :: IO (Either CLIDiagnostic JupyterEnvironment)
detectJupyterEnvironment = withErrorContext "jupyter-environment-detection" $ do
  -- Constitutional resource limits for environment detection (<5s timeout)
  let detectionLimits = defaultResourceLimits
        { rcMaxCpuSeconds = 5.0    -- 5 second timeout for environment detection
        , rcMaxMemoryMB = 25       -- 25MB memory limit for detection operations
        , rcMaxOutputBytes = 524288  -- 512KB output limit for command outputs
        }
  
  withResourceGuard detectionLimits $ \guard -> do
    -- Use the Utilities module function for core detection
    result <- withCLIResourceError "core-jupyter-detection" Utilities.detectJupyterEnvironment
    case result of
      Left diag -> return $ Left diag
      Right jupyterEnv -> do
        -- Enhanced detection with additional validation (no resource wrapper needed - pure function)
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

-- ===========================================================================
-- T017: Kernel.json Generation with Constitutional Compliance
-- ===========================================================================

-- | Generate kernel.json content for HsJupyter kernel installation (T017)
generateKernelJson :: InstallOptions -> FilePath -> IO (Either CLIDiagnostic Value)
generateKernelJson options ghcPath = do
  -- Constitutional validation: validate inputs
  if null ghcPath
    then return $ Left $ ValidationError "GHC path cannot be empty"
    else do
      -- Get the path to the hs-jupyter-kernel executable
      kernelExecutablePath <- getKernelExecutablePath
      case kernelExecutablePath of
        Left diag -> return $ Left diag
        Right executablePath -> do
          -- Build kernel.json structure following Jupyter specification
          let kernelJson = object
                [ "argv" .= generateKernelArgv executablePath
                , "display_name" .= getDisplayName options
                , "language" .= ("haskell" :: Text)
                , "interrupt_mode" .= ("signal" :: Text)
                , "env" .= generateEnvironmentVariables options ghcPath
                , "metadata" .= generateKernelMetadata options
                ]
          return $ Right kernelJson

-- | Generate argv array for kernel startup command (T017)
generateKernelArgv :: FilePath -> [Text]
generateKernelArgv executablePath =
  [ T.pack executablePath
  , "--connection"
  , "{connection_file}"  -- Jupyter will substitute this placeholder
  ]

-- | Get display name for the kernel from options or default (T017)
getDisplayName :: InstallOptions -> Text
getDisplayName options = fromMaybe "Haskell" (ioDisplayName options)

-- | Generate environment variables for kernel execution (T017)
generateEnvironmentVariables :: InstallOptions -> FilePath -> Object
generateEnvironmentVariables _options ghcPath = 
  KM.fromList [(K.fromText "GHC_PATH", String $ T.pack ghcPath)]

-- | Generate kernel metadata with constitutional compliance (T017)
generateKernelMetadata :: InstallOptions -> Object
generateKernelMetadata _options = KM.fromList
  [ (K.fromText "kernel_version", String "0.1.0.0")
  , (K.fromText "implementation", String "hs-jupyter-kernel")
  , (K.fromText "implementation_version", String "0.1.0.0")
  , (K.fromText "language_version", String "GHC 9.12.2+")
  , (K.fromText "banner", String "HsJupyter - Haskell kernel for Jupyter notebooks")
  , (K.fromText "help_links", Array $ V.fromList
      [ object
          [ ("text", String "HsJupyter Documentation")
          , ("url", String "https://github.com/user/HsJupyter")
          ]
      ])
  ]

-- | Get the path to the hs-jupyter-kernel executable (T017)
getKernelExecutablePath :: IO (Either CLIDiagnostic FilePath)
getKernelExecutablePath = do
  -- Try to find the executable in common locations
  result <- try $ do
    -- First try to use the same executable that's currently running
    executablePath <- getExecutablePath
    return executablePath
  case result of
    Left (_ :: IOException) -> do
      -- Fallback: try to find hs-jupyter-kernel in PATH
      pathResult <- findExecutable "hs-jupyter-kernel"
      case pathResult of
        Nothing -> return $ Left $ ValidationError "Unable to locate hs-jupyter-kernel executable"
        Just execPath -> return $ Right execPath
    Right execPath -> return $ Right execPath

-- | Write kernel.json file to the specified path with constitutional error handling (T017)
writeKernelJson :: FilePath -> Value -> IO (Either CLIDiagnostic ())
writeKernelJson kernelPath kernelJson = do
  result <- try $ do
    -- Ensure the directory exists
    let kernelDir = takeDirectory kernelPath
    createDirectoryIfMissing True kernelDir
    
    -- Write kernel.json with standard JSON formatting
    LBS.writeFile kernelPath $ encode kernelJson
  
  case result of
    Left (ex :: IOException) -> 
      return $ Left $ ValidationError $ "Failed to write kernel.json: " <> T.pack (show ex)
    Right () -> return $ Right ()

-- | Complete kernel installation by generating and writing kernel.json (T017)
installKernelJson :: InstallOptions -> FilePath -> FilePath -> IO (Either CLIDiagnostic FilePath)
installKernelJson options kernelPath ghcPath = withErrorContext "kernel-json-installation" $ do
  -- Step 1: Generate kernel.json content with resource protection
  jsonResult <- withCLIResourceError "json-generation" $ generateKernelJson options ghcPath
  case jsonResult of
    Left diag -> return $ Left diag
    Right kernelJson -> do
      -- Step 2: Write kernel.json to file with cleanup on failure
      writeResult <- withCLIResourceCleanup (cleanupJsonFile kernelPath) $
        withCLIResourceError "json-file-write" $ writeKernelJson kernelPath kernelJson
      case writeResult of
        Left diag -> return $ Left diag
        Right () -> return $ Right kernelPath
  where
    -- Cleanup incomplete kernel.json files on failure
    cleanupJsonFile :: FilePath -> IO ()
    cleanupJsonFile path = do
      fileExists <- doesFileExist path
      when fileExists $ do
        (try :: IO () -> IO (Either SomeException ())) (removeDirectoryRecursive (takeDirectory path)) >> return ()
      return ()

-- | Validate generated kernel.json content against Jupyter requirements (T017)
validateKernelJson :: Value -> IO (Either CLIDiagnostic Value)
validateKernelJson kernelJson = do
  case kernelJson of
    Object obj -> do
      -- Check required fields according to Jupyter kernel specification
      case (KM.lookup (K.fromText "argv") obj, KM.lookup (K.fromText "display_name") obj, KM.lookup (K.fromText "language") obj) of
        (Just (Array _), Just (String _), Just (String _)) -> 
          return $ Right kernelJson
        _ -> return $ Left $ ValidationError "Invalid kernel.json: missing required fields (argv, display_name, language)"
    _ -> return $ Left $ ValidationError "Invalid kernel.json: root must be an object"

-- ===========================================================================
-- T018: Kernel Registration and File System Operations
-- ===========================================================================

-- | Execute complete kernel registration workflow (T018)
executeKernelRegistration :: InstallOptions -> JupyterEnvironment -> IO (Either CLIDiagnostic FilePath)
executeKernelRegistration options jupyterEnv = withErrorContext "kernel-registration-workflow" $ do
  -- Constitutional resource limits for registration operations
  let registrationLimits = defaultResourceLimits
        { rcMaxCpuSeconds = 30.0   -- 30 second timeout for registration steps
        , rcMaxMemoryMB = 50       -- 50MB memory limit for file operations
        , rcMaxOutputBytes = 1048576  -- 1MB output limit for logs
        }
  
  withResourceGuard registrationLimits $ \guard -> do
    -- Step 1: Find suitable kernelspec directory for installation  
    targetDirectoryResult <- withCLIResourceError "directory-selection" $ 
      selectInstallationDirectory options jupyterEnv
    case targetDirectoryResult of
      Left diag -> return $ Left diag
      Right targetDir -> do
        -- Step 2: Determine kernel name and check for conflicts
        kernelNameResult <- withCLIResourceError "kernel-name-resolution" $ 
          resolveKernelName options targetDir
        case kernelNameResult of
          Left diag -> return $ Left diag
          Right kernelName -> do
            -- Step 3: Get GHC path for kernel configuration
            ghcPathResult <- withCLIResourceError "ghc-path-resolution" $ 
              resolveGHCPath options
            case ghcPathResult of
              Left diag -> return $ Left diag
              Right ghcPath -> do
                -- Step 4: Create kernel directory and install kernel.json with cleanup
                let kernelPath = getKernelPath targetDir kernelName
                installResult <- withCLIResourceCleanup (cleanupOnFailure kernelPath) $
                  withCLIResourceError "kernel-json-installation" $ 
                    installKernelJson options kernelPath ghcPath
                case installResult of
                  Left diag -> return $ Left diag
                  Right installedPath -> do
                    -- Step 5: Verify installation success
                    verificationResult <- withCLIResourceError "installation-verification" $ 
                      verifyKernelInstallation installedPath
                    case verificationResult of
                      Left diag -> return $ Left diag
                      Right _ -> return $ Right installedPath

-- | Cleanup function for failed installations (T019: Constitutional resource cleanup)
cleanupOnFailure :: FilePath -> IO ()
cleanupOnFailure kernelPath = do
  dirExists <- doesDirectoryExist (takeDirectory kernelPath)
  when dirExists $ do
    -- Remove incomplete kernel directory if it exists
    (try :: IO () -> IO (Either SomeException ())) (removeDirectoryRecursive (takeDirectory kernelPath)) >> return ()
  return ()

-- | Select the most appropriate installation directory from available options (T018)
selectInstallationDirectory :: InstallOptions -> JupyterEnvironment -> IO (Either CLIDiagnostic FilePath)
selectInstallationDirectory options jupyterEnv = do
  let availableDirs = jeKernelspecDirs jupyterEnv
  
  case ioScope options of
    AutoDetect -> do
      -- Choose the first writable directory, preferring user directories
      suitableDir <- findBestInstallationDirectory availableDirs
      case suitableDir of
        Nothing -> return $ Left $ ValidationError "No suitable installation directory found"
        Just dir -> return $ Right dir
        
    UserInstallation -> do
      -- Filter to user directories only
      userDirs <- filterUserDirectories availableDirs
      case userDirs of
        [] -> return $ Left $ ValidationError "No user-accessible kernelspec directories found"
        (dir:_) -> return $ Right dir
        
    SystemInstallation -> do
      -- Filter to system directories only
      systemDirs <- filterSystemDirectories availableDirs
      case systemDirs of
        [] -> return $ Left $ ValidationError "No system kernelspec directories found or insufficient permissions"
        (dir:_) -> return $ Right dir
        
    CustomPath customDir -> do
      -- Validate custom directory exists and is writable
      customValidation <- filterAccessibleDirectories [customDir]
      case customValidation of
        [] -> return $ Left $ ValidationError $ "Custom directory not accessible: " <> T.pack customDir
        (_:_) -> return $ Right customDir

-- | Find the best installation directory from available options (T018)
findBestInstallationDirectory :: [FilePath] -> IO (Maybe FilePath)
findBestInstallationDirectory [] = return Nothing
findBestInstallationDirectory dirs = do
  -- Prefer user directories over system directories
  userDirs <- filterUserDirectories dirs
  if not (null userDirs)
    then return $ Just (head userDirs)
    else do
      -- Fall back to system directories if no user directories available
      systemDirs <- filterSystemDirectories dirs
      return $ if null systemDirs then Nothing else Just (head systemDirs)

-- | Resolve kernel name, handling conflicts with existing installations (T018)
resolveKernelName :: InstallOptions -> FilePath -> IO (Either CLIDiagnostic Text)
resolveKernelName options targetDir = do
  let baseKernelName = fromMaybe "haskell" (ioDisplayName options)
      proposedName = T.toLower $ T.replace " " "-" baseKernelName
  
  if ioForceReinstall options
    then do
      -- Force reinstall: remove existing installation if present
      let kernelDir = targetDir </> T.unpack proposedName
      kernelDirExists <- doesDirectoryExist kernelDir
      when kernelDirExists $ do
        result <- try $ removeDirectoryRecursive kernelDir
        case result of
          Left (_ :: IOException) -> return ()  -- Ignore removal errors for now
          Right () -> return ()
      return $ Right proposedName
    else do
      -- Check for conflicts and generate unique name if needed
      finalName <- generateUniqueKernelName targetDir proposedName
      return $ Right finalName

-- | Generate a unique kernel name if conflicts exist (T018)
generateUniqueKernelName :: FilePath -> Text -> IO Text
generateUniqueKernelName targetDir baseName = do
  let baseDir = targetDir </> T.unpack baseName
  baseExists <- doesDirectoryExist baseDir
  if not baseExists
    then return baseName
    else findAvailableName baseName 1
  where
    findAvailableName :: Text -> Int -> IO Text
    findAvailableName base counter = do
      let candidateName = base <> "-" <> T.pack (show counter)
          candidateDir = targetDir </> T.unpack candidateName
      candidateExists <- doesDirectoryExist candidateDir
      if not candidateExists
        then return candidateName
        else findAvailableName base (counter + 1)

-- | Resolve GHC path for kernel configuration (T018)
resolveGHCPath :: InstallOptions -> IO (Either CLIDiagnostic FilePath)
resolveGHCPath options = do
  case ioGHCPath options of
    Just customGHCPath -> 
      -- Use custom GHC path if provided
      return $ Right customGHCPath
    Nothing -> do
      -- Auto-detect GHC path
      ghcPathResult <- findExecutable "ghc"
      case ghcPathResult of
        Nothing -> return $ Left $ ValidationError "GHC executable not found in PATH. Please specify --ghc-path or ensure GHC is installed."
        Just ghcPath -> return $ Right ghcPath

-- | Verify that kernel installation was successful (T018)
verifyKernelInstallation :: FilePath -> IO (Either CLIDiagnostic ())
verifyKernelInstallation kernelPath = do
  -- Check that kernel.json file exists and is readable
  kernelExists <- doesFileExist kernelPath
  if not kernelExists
    then return $ Left $ ValidationError $ "Kernel installation failed: kernel.json not found at " <> T.pack kernelPath
    else do
      -- Try to parse the kernel.json file to ensure it's valid
      result <- try $ LBS.readFile kernelPath
      case result of
        Left (_ :: IOException) -> 
          return $ Left $ ValidationError $ "Kernel installation verification failed: cannot read " <> T.pack kernelPath
        Right content -> do
          case eitherDecode content of
            Left parseError -> 
              return $ Left $ ValidationError $ "Kernel installation verification failed: invalid JSON in " <> T.pack kernelPath <> ": " <> T.pack parseError
            Right (kernelJson :: Value) -> do
              -- Validate the kernel.json structure
              validationResult <- validateKernelJson kernelJson
              case validationResult of
                Left diag -> return $ Left diag
                Right _ -> return $ Right ()