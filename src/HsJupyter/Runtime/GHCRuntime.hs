{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsJupyter.Runtime.GHCRuntime
  ( -- * Core evaluation functions
    evaluateExpression
  , evaluateDeclaration  
  , importModule
    
    -- * Cancellable evaluation functions
  , evaluateExpressionCancellable
  , evaluateDeclarationCancellable
  , importModuleCancellable
    
    -- * ResourceGuard-monitored evaluation functions
  , evaluateExpressionGuarded
  , evaluateDeclarationGuarded
  , importModuleGuarded
    
    -- * Memory-limited evaluation functions
  , evaluateExpressionMemoryLimited
  , evaluateDeclarationMemoryLimited
  , importModuleMemoryLimited
    
    -- * Performance-monitored evaluation functions
  , evaluateExpressionMonitored
  , evaluateDeclarationMonitored
  , importModuleMonitored
    
    -- * Cancellation support
  , CancellationToken(..)
  , newCancellationToken
  , cancel
  , checkCancellation
    
    -- * ResourceGuard integration
  , resourceBudgetToLimits
  , convertResourceViolation
    
    -- * Memory monitoring
  , MemoryStats(..)
  , getCurrentMemoryStats
  , checkMemoryLimits
  , withMemoryLimit
    
    -- * Performance monitoring and telemetry
  , PerformanceTelemetry(..)
  , PerformanceMetrics(..)
  , PerformanceTracker(..)
  , newPerformanceTracker
  , recordTelemetry
  , updateMetrics
  , withPerformanceMonitoring
    
    -- * Session management
  , initializeGHCSession
  , resetGHCSession
  , getSessionBindings
    
    -- * Configuration
  , GHCEvaluationRequest(..)
  , GHCEvaluationResult(..)
  , EvaluationType(..)
  , defaultGHCConfig
  ) where

import Control.Concurrent.STM
-- TMVar import removed: not needed here
import Control.Exception (catch, evaluate)
import Control.Monad.IO.Class
import Data.Char (isDigit, isSpace)
import Data.Int (Int64)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime, UTCTime)
import GHC.Stats (getRTSStats, RTSStats(..))
import System.Timeout (timeout)
import Language.Haskell.Interpreter (runInterpreter, interpret, as, setImports, runStmt)

import HsJupyter.Runtime.GHCSession (GHCSessionState(..), GHCConfig(..), ImportPolicy(..), ImportDefault(..), newGHCSession, cleanupSession, listBindings, listDeclarations, addDeclaration, extractBindingNames, addBinding, addImportedModule, listImportedModules, checkImportPolicy, defaultSafeModules)
import HsJupyter.Runtime.GHCDiagnostics (GHCError(..), SourceLocation(..), interpretError)
import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic)
import HsJupyter.Runtime.SessionState (ResourceBudget(..))
import HsJupyter.Runtime.ResourceGuard (ResourceLimits(..), ResourceViolation(..), withResourceGuard, CpuLimitMode(..), MemoryLimitMode(..))

-- | Input structure for GHC evaluation operations
data GHCEvaluationRequest = GHCEvaluationRequest
  { reqCode :: Text           -- Haskell code to evaluate
  , reqType :: EvaluationType -- Expression | Declaration | Import
  , reqSessionId :: Text      -- Identifies the target session
  , reqTimeout :: Maybe Int   -- Optional custom timeout
  } deriving (Show, Eq)

-- | Output structure for GHC evaluation results
data GHCEvaluationResult = GHCEvaluationResult
  { resSuccess :: Bool            -- Whether evaluation succeeded
  , resResult :: Maybe Text       -- Evaluated result for expressions
  , resOutput :: Text            -- Stdout/stderr output
  , resDiagnostics :: [RuntimeDiagnostic] -- Errors, warnings, info messages
  , resBindingsAdded :: [String]  -- New variable/function bindings created
  , resModulesImported :: [String] -- New modules successfully imported
  } deriving (Show, Eq)

-- | Cancellation token for GHC operations  
data CancellationToken = CancellationToken
  { cancelSignal :: TMVar ()      -- Signal for cancellation
  , isCancelled :: TMVar Bool     -- Current cancellation state
  } 

-- | Create a new cancellation token
newCancellationToken :: STM CancellationToken
newCancellationToken = do
  signal <- newEmptyTMVar
  cancelled <- newTMVar False
  return $ CancellationToken signal cancelled

-- | Cancel an operation
cancel :: CancellationToken -> STM ()
cancel token = do
  writeTMVar (cancelSignal token) ()
  writeTMVar (isCancelled token) True

-- | Check if operation is cancelled  
checkCancellation :: CancellationToken -> STM Bool
checkCancellation token = readTMVar (isCancelled token)

-- | Convert ResourceBudget to ResourceLimits
resourceBudgetToLimits :: ResourceBudget -> ResourceLimits
resourceBudgetToLimits budget = ResourceLimits
  { rcMaxCpuSeconds = realToFrac (rbCpuTimeout budget)
  , rcMaxMemoryMB = fromIntegral (rbMemoryLimit budget) `div` (1024 * 1024)  -- Convert bytes to MB
  , rcMaxOutputBytes = fromIntegral (rbMaxStreamBytes budget)
  , rcCpuMode = CpuWall
  , rcMemoryMode = MemoryResident
  }

-- | Memory usage tracking information
data MemoryStats = MemoryStats
  { msAllocatedBytes :: Int64    -- Total allocated bytes
  , msResidentBytes :: Int64     -- Current resident memory
  , msMaxResidentBytes :: Int64  -- Peak memory usage
  } deriving (Show, Eq)

-- | Performance telemetry data
data PerformanceTelemetry = PerformanceTelemetry
  { ptOperationType :: EvaluationType      -- Type of operation performed
  , ptExecutionTime :: NominalDiffTime     -- Time taken to execute
  , ptMemoryBefore :: MemoryStats         -- Memory stats before execution
  , ptMemoryAfter :: MemoryStats          -- Memory stats after execution
  , ptSuccess :: Bool                     -- Whether operation succeeded
  , ptErrorType :: Maybe String           -- Type of error if failed
  , ptCodeLength :: Int                   -- Length of code being executed
  , ptTimestamp :: UTCTime               -- When the operation occurred
  } deriving (Show, Eq)

-- | Performance metrics aggregation
data PerformanceMetrics = PerformanceMetrics
  { pmTotalOperations :: Int              -- Total number of operations
  , pmSuccessCount :: Int                 -- Number of successful operations
  , pmFailureCount :: Int                 -- Number of failed operations
  , pmAverageExecutionTime :: NominalDiffTime -- Average execution time
  , pmTotalMemoryAllocated :: Int64       -- Total memory allocated
  , pmExpressionCount :: Int              -- Count of expression evaluations
  , pmDeclarationCount :: Int             -- Count of declaration evaluations
  , pmImportCount :: Int                  -- Count of import operations
  } deriving (Show, Eq)

-- | Global performance tracking state
data PerformanceTracker = PerformanceTracker
  { ptTelemetryLog :: [PerformanceTelemetry]  -- Recent telemetry entries
  , ptMetrics :: PerformanceMetrics          -- Aggregated metrics
  , ptMaxLogSize :: Int                      -- Maximum log entries to keep
  } deriving (Show, Eq)

-- | Get current memory statistics
getCurrentMemoryStats :: IO MemoryStats
getCurrentMemoryStats = do
  stats <- getRTSStats
  return $ MemoryStats
    { msAllocatedBytes = fromIntegral $ allocated_bytes stats
    , msResidentBytes = fromIntegral $ max_live_bytes stats  -- Use max_live_bytes as approximation
    , msMaxResidentBytes = fromIntegral $ max_live_bytes stats
    }

-- | Check if memory usage exceeds limits
checkMemoryLimits :: ResourceBudget -> MemoryStats -> Either GHCError ()
checkMemoryLimits budget stats = 
  let limitBytes = rbMemoryLimit budget
      currentBytes = msResidentBytes stats
  in if currentBytes > limitBytes
    then Left $ CompilationError 
           ("Memory limit exceeded: " <> T.pack (show (currentBytes `div` (1024*1024))) <> "MB > " <> T.pack (show (limitBytes `div` (1024*1024))) <> "MB")
           (SourceLocation 1 1 Nothing)
           ["Reduce memory usage", "Simplify computation", "Use lazy evaluation"]
    else Right ()

-- | Execute action with memory monitoring
withMemoryLimit :: ResourceBudget -> IO a -> IO (Either GHCError a)
withMemoryLimit budget action = do
  -- only collect final stats to determine if the action violated limits
  result <- action
  finalStats <- getCurrentMemoryStats

  -- Force evaluation to ensure memory is actually used
  _ <- evaluate result

  case checkMemoryLimits budget finalStats of
    Left err -> return $ Left err
    Right () -> return $ Right result

-- | Create initial performance tracker
newPerformanceTracker :: Int -> PerformanceTracker
newPerformanceTracker maxLogSize = PerformanceTracker
  { ptTelemetryLog = []
  , ptMetrics = initialMetrics
  , ptMaxLogSize = maxLogSize
  }
  where
    initialMetrics = PerformanceMetrics
      { pmTotalOperations = 0
      , pmSuccessCount = 0
      , pmFailureCount = 0
      , pmAverageExecutionTime = 0
      , pmTotalMemoryAllocated = 0
      , pmExpressionCount = 0
      , pmDeclarationCount = 0
      , pmImportCount = 0
      }

-- | Record performance telemetry
recordTelemetry :: PerformanceTracker -> PerformanceTelemetry -> PerformanceTracker
recordTelemetry tracker telemetry = 
  let newLog = take (ptMaxLogSize tracker) (telemetry : ptTelemetryLog tracker)
      oldMetrics = ptMetrics tracker
      newMetrics = updateMetrics oldMetrics telemetry
  in tracker { ptTelemetryLog = newLog, ptMetrics = newMetrics }

-- | Update aggregated metrics with new telemetry
updateMetrics :: PerformanceMetrics -> PerformanceTelemetry -> PerformanceMetrics
updateMetrics metrics telemetry = 
  let newTotal = pmTotalOperations metrics + 1
      newSuccess = if ptSuccess telemetry then pmSuccessCount metrics + 1 else pmSuccessCount metrics
      newFailures = if ptSuccess telemetry then pmFailureCount metrics else pmFailureCount metrics + 1
      totalTime = pmAverageExecutionTime metrics * fromIntegral (pmTotalOperations metrics) + ptExecutionTime telemetry
      newAvgTime = totalTime / fromIntegral newTotal
      memoryDelta = msAllocatedBytes (ptMemoryAfter telemetry) - msAllocatedBytes (ptMemoryBefore telemetry)
      newTotalMemory = pmTotalMemoryAllocated metrics + max 0 memoryDelta
      (newExpr, newDecl, newImport) = case ptOperationType telemetry of
        Expression -> (pmExpressionCount metrics + 1, pmDeclarationCount metrics, pmImportCount metrics)
        Declaration -> (pmExpressionCount metrics, pmDeclarationCount metrics + 1, pmImportCount metrics)
        Import -> (pmExpressionCount metrics, pmDeclarationCount metrics, pmImportCount metrics + 1)
  in metrics
    { pmTotalOperations = newTotal
    , pmSuccessCount = newSuccess
    , pmFailureCount = newFailures
    , pmAverageExecutionTime = newAvgTime
    , pmTotalMemoryAllocated = newTotalMemory
    , pmExpressionCount = newExpr
    , pmDeclarationCount = newDecl
    , pmImportCount = newImport
    }

-- | Execute action with performance monitoring
withPerformanceMonitoring :: EvaluationType -> Text -> IO (Either GHCError a) -> IO (Either GHCError a, PerformanceTelemetry)
withPerformanceMonitoring opType code action = do
  startTime <- getCurrentTime
  memoryBefore <- getCurrentMemoryStats
  
  result <- action
  
  endTime <- getCurrentTime
  memoryAfter <- getCurrentMemoryStats
  
  let executionTime = diffUTCTime endTime startTime
      success = case result of
        Left _ -> False
        Right _ -> True
      errorType = case result of
        Left (CompilationError msg _ _) -> Just ("CompilationError: " ++ T.unpack (T.take 50 msg))
        Left (ImportError _ msg) -> Just ("ImportError: " ++ T.unpack (T.take 50 msg))
        Left (TimeoutError secs) -> Just ("TimeoutError: " ++ show secs ++ "s")
        Left (RuntimeError msg) -> Just ("RuntimeError: " ++ T.unpack (T.take 50 msg))
        Left (SecurityError msg) -> Just ("SecurityError: " ++ T.unpack (T.take 50 msg))
        Right _ -> Nothing
      
      telemetry = PerformanceTelemetry
        { ptOperationType = opType
        , ptExecutionTime = executionTime
        , ptMemoryBefore = memoryBefore
        , ptMemoryAfter = memoryAfter
        , ptSuccess = success
        , ptErrorType = errorType
        , ptCodeLength = T.length code
        , ptTimestamp = startTime
        }
  
  return (result, telemetry)

-- | Convert ResourceViolation to GHCError
convertResourceViolation :: ResourceViolation -> Text -> GHCError
convertResourceViolation violation _code = case violation of
  TimeoutViolation _elapsed limit ->
    TimeoutError (ceiling limit)
  MemoryViolation used limit ->
    CompilationError ("Memory limit exceeded: " <> T.pack (show used) <> "MB > " <> T.pack (show limit) <> "MB")
                     (SourceLocation 1 1 Nothing)
                     ["Reduce memory usage", "Simplify computation"]
  OutputViolation size limit ->
    CompilationError ("Output limit exceeded: " <> T.pack (show size) <> " bytes > " <> T.pack (show limit) <> " bytes")
                     (SourceLocation 1 1 Nothing)
                     ["Reduce output size", "Use take or similar to limit results"]
  ExecutionError msg ->
    RuntimeError (T.pack msg)

-- | Type of evaluation to perform
data EvaluationType
  = Expression      -- Return computed value
  | Declaration     -- Define variable/function
  | Import         -- Import module
  deriving (Show, Eq)

-- | Evaluate a Haskell expression with intelligent timeout selection
evaluateExpression :: GHCSessionState -> Text -> IO (Either GHCError Text)
evaluateExpression session code = do
  token <- atomically newCancellationToken
  evaluateExpressionCancellable session code token

-- | Evaluate a Haskell expression with performance monitoring
evaluateExpressionMonitored :: GHCSessionState -> Text -> IO (Either GHCError Text, PerformanceTelemetry)
evaluateExpressionMonitored session code = 
  withPerformanceMonitoring Expression code $ do
    token <- atomically newCancellationToken
    evaluateExpressionCancellable session code token

-- | Evaluate a Haskell expression with memory limit enforcement
evaluateExpressionMemoryLimited :: GHCSessionState -> Text -> IO (Either GHCError Text)
evaluateExpressionMemoryLimited session code = do
  let budget = resourceLimits $ sessionConfig session
  result <- withMemoryLimit budget $ do
    token <- atomically newCancellationToken
    evaluateExpressionCancellable session code token
  case result of
    Left err -> return $ Left err
    Right (Left err) -> return $ Left err
    Right (Right value) -> return $ Right value

-- | Evaluate a Haskell expression with ResourceGuard monitoring
evaluateExpressionGuarded :: GHCSessionState -> Text -> IO (Either GHCError Text)
evaluateExpressionGuarded session code = do
  let resourceLims = resourceBudgetToLimits (resourceLimits $ sessionConfig session)
  result <- withResourceGuard resourceLims $ \_guard -> do
    token <- atomically newCancellationToken
    evaluateExpressionCancellable session code token
      `catch` \(violation :: ResourceViolation) -> 
        return $ Left (convertResourceViolation violation code)
  return result

-- | Evaluate a Haskell expression with cancellation support
evaluateExpressionCancellable :: GHCSessionState -> Text -> CancellationToken -> IO (Either GHCError Text)
evaluateExpressionCancellable session code token = do
  let timeoutSeconds = selectTimeout session Expression code
  result <- timeout (timeoutSeconds * 1000000) $ do
    -- Check for cancellation before starting
    cancelled <- atomically $ checkCancellation token
    if cancelled 
      then return (Left (CompilationError "Operation was cancelled" (SourceLocation 1 1 Nothing) []))
      else do
        -- Support simple GHCi-style import commands (e.g. ":m + Data.List" or ":module + Data.List")
        if ":" `T.isPrefixOf` code
          then do
            let cmd = T.unpack $ T.strip code
            case parseGHCiImport cmd of
              Just importStmt -> do
                -- perform import using existing import routine which understands policy and side-effects
                imRes <- importModuleCancellable session importStmt token
                case imRes of
                  Left err -> return $ Left err
                  Right () -> return $ Right "" -- import succeeded, return empty string as expression result
              Nothing -> return $ Left (CompilationError (T.pack ("Unsupported GHCi command: " ++ cmd)) (SourceLocation 1 1 Nothing) [])
          else do
            -- Get current session state to restore bindings and imports
            decls <- atomically $ listDeclarations session
            imports <- atomically $ listImportedModules session
            
            interpreterResult <- runInterpreter $ do
              -- Set up imports (including previously imported modules)
              let allImports = "Prelude" : imports
              setImports allImports
              
              -- Restore previous bindings by re-running declarations
              mapM_ runStmt decls
              
              -- Wrap expression with 'show' to get String representation
              let wrappedCode = "show (" ++ T.unpack code ++ ")"
              interpret wrappedCode (as :: String)
            return $ case interpreterResult of
              Left err -> Left (interpretError err)
              Right value -> Right value
  case result of
    Nothing -> do
      -- Check if timeout or cancellation
      cancelled <- atomically $ checkCancellation token
      if cancelled
        then return $ Left (CompilationError "Operation was cancelled" (SourceLocation 1 1 Nothing) [])
        else return $ Left (TimeoutError timeoutSeconds)  -- Timeout occurred
    Just (Left err) -> return $ Left err   -- Already converted interpreter error  
    Just (Right value) -> return $ Right (T.pack value)     -- Success

-- | Execute a Haskell declaration with intelligent timeout selection
evaluateDeclaration :: GHCSessionState -> Text -> IO (Either GHCError [String])
evaluateDeclaration session code = do
  token <- atomically newCancellationToken  
  evaluateDeclarationCancellable session code token

-- | Execute a Haskell declaration with performance monitoring
evaluateDeclarationMonitored :: GHCSessionState -> Text -> IO (Either GHCError [String], PerformanceTelemetry)
evaluateDeclarationMonitored session code = 
  withPerformanceMonitoring Declaration code $ do
    token <- atomically newCancellationToken
    evaluateDeclarationCancellable session code token

-- | Execute a Haskell declaration with memory limit enforcement
evaluateDeclarationMemoryLimited :: GHCSessionState -> Text -> IO (Either GHCError [String])
evaluateDeclarationMemoryLimited session code = do
  let budget = resourceLimits $ sessionConfig session
  result <- withMemoryLimit budget $ do
    token <- atomically newCancellationToken
    evaluateDeclarationCancellable session code token
  case result of
    Left err -> return $ Left err
    Right (Left err) -> return $ Left err
    Right (Right bindings) -> return $ Right bindings

-- | Execute a Haskell declaration with ResourceGuard monitoring
evaluateDeclarationGuarded :: GHCSessionState -> Text -> IO (Either GHCError [String])
evaluateDeclarationGuarded session code = do
  let resourceLims = resourceBudgetToLimits (resourceLimits $ sessionConfig session)
  result <- withResourceGuard resourceLims $ \_guard -> do
    token <- atomically newCancellationToken
    evaluateDeclarationCancellable session code token
      `catch` \(violation :: ResourceViolation) ->
        return $ Left (convertResourceViolation violation code)
  return result

-- | Execute a Haskell declaration with cancellation support
evaluateDeclarationCancellable :: GHCSessionState -> Text -> CancellationToken -> IO (Either GHCError [String])
evaluateDeclarationCancellable session code token = do
  let timeoutSeconds = selectTimeout session Declaration code
  result <- timeout (timeoutSeconds * 1000000) $ do
    -- Check for cancellation before starting
    cancelled <- atomically $ checkCancellation token
    if cancelled 
      then return (Left (CompilationError "Operation was cancelled" (SourceLocation 1 1 Nothing) []))
      else do
        -- Get current session state to restore bindings and imports
        decls <- atomically $ listDeclarations session
        imports <- atomically $ listImportedModules session
        
        interpreterResult <- runInterpreter $ do
          -- Set up imports (including previously imported modules)
          let allImports = "Prelude" : imports
          setImports allImports
          
          -- Restore previous bindings by re-running declarations
          mapM_ runStmt decls
          
          -- Execute the new declaration using runStmt (for let bindings, function definitions)
          runStmt (T.unpack code)
        return $ case interpreterResult of
          Left err -> Left (interpretError err)
          Right value -> Right value
  case result of
    Nothing -> do
      -- Check if timeout or cancellation
      cancelled <- atomically $ checkCancellation token
      if cancelled
        then return $ Left (CompilationError "Operation was cancelled" (SourceLocation 1 1 Nothing) [])
        else return $ Left (TimeoutError timeoutSeconds)  -- Timeout occurred
    Just (Left err) -> return $ Left err   -- Already converted interpreter error
    Just (Right _) -> do
      -- Extract binding names and update session state
      let bindingNames = extractBindingNames code
          declCode = T.unpack code
      atomically $ do
        mapM_ (addBinding session) bindingNames
        addDeclaration session declCode  -- Store the full declaration for replay
      return $ Right bindingNames

-- | Import a Haskell module with security policy checking
-- Supports: import ModuleName, import qualified ModuleName, import qualified ModuleName as Alias
importModule :: GHCSessionState -> String -> IO (Either GHCError ())
importModule session importStatement = do
  token <- atomically newCancellationToken
  importModuleCancellable session importStatement token

-- | Import a Haskell module with performance monitoring
importModuleMonitored :: GHCSessionState -> String -> IO (Either GHCError (), PerformanceTelemetry)
importModuleMonitored session importStatement = 
  withPerformanceMonitoring Import (T.pack importStatement) $ do
    token <- atomically newCancellationToken
    importModuleCancellable session importStatement token

-- | Import a Haskell module with memory limit enforcement
importModuleMemoryLimited :: GHCSessionState -> String -> IO (Either GHCError ())
importModuleMemoryLimited session importStatement = do
  let budget = resourceLimits $ sessionConfig session
  result <- withMemoryLimit budget $ do
    token <- atomically newCancellationToken
    importModuleCancellable session importStatement token
  case result of
    Left err -> return $ Left err
    Right (Left err) -> return $ Left err
    Right (Right ()) -> return $ Right ()

-- | Import a Haskell module with ResourceGuard monitoring  
importModuleGuarded :: GHCSessionState -> String -> IO (Either GHCError ())
importModuleGuarded session importStatement = do
  let resourceLims = resourceBudgetToLimits (resourceLimits $ sessionConfig session)
  result <- withResourceGuard resourceLims $ \_guard -> do
    token <- atomically newCancellationToken
    importModuleCancellable session importStatement token
      `catch` \(violation :: ResourceViolation) ->
        return $ Left (convertResourceViolation violation (T.pack importStatement))
  return result

-- | Import a Haskell module with cancellation support
importModuleCancellable :: GHCSessionState -> String -> CancellationToken -> IO (Either GHCError ())
importModuleCancellable session importStatement token = do
  -- Validate import statement first
  case validateImportStatement importStatement of
    Left err -> return $ Left (ImportError "invalid" (T.pack err))
    Right () -> do
      -- Parse import statement to extract module name
      let (moduleName, _isQualified) = parseImportStatement importStatement
      
      -- Check if module is allowed by policy
      policyCheck <- atomically $ checkImportPolicy session moduleName
      case policyCheck of
        Left err -> return $ Left (ImportError moduleName (T.pack err))
        Right () -> do
          -- Execute import with intelligent timeout selection and cancellation support
          let timeoutSeconds = selectTimeout session Import (T.pack importStatement)
          result <- timeout (timeoutSeconds * 1000000) $ do
            -- Check for cancellation before starting
            cancelled <- atomically $ checkCancellation token
            if cancelled 
              then return (Left (CompilationError "Operation was cancelled" (SourceLocation 1 1 Nothing) []))
              else do
                -- Get current session state to restore
                currentDecls <- atomically $ listDeclarations session
                currentImports <- atomically $ listImportedModules session
                
                interpreterResult <- runInterpreter $ do
                  -- Set up imports (including the new module)
                  let allImports = "Prelude" : currentImports ++ [moduleName]
                  setImports allImports
                  
                  -- Restore previous declarations
                  mapM_ runStmt currentDecls
                  
                  return ()
                return $ case interpreterResult of
                  Left err -> Left (interpretError err)
                  Right value -> Right value
          
          case result of
            Nothing -> do
              -- Check if timeout or cancellation
              cancelled <- atomically $ checkCancellation token
              if cancelled
                then return $ Left (CompilationError "Operation was cancelled" (SourceLocation 1 1 Nothing) [])
                else return $ Left (TimeoutError timeoutSeconds)
            Just (Left err) -> return $ Left err   -- Already converted interpreter error
            Just (Right ()) -> do
              -- Add to imported modules list (store canonical module name)
              atomically $ addImportedModule session moduleName
              return $ Right ()

-- | Initialize a new GHC session with configuration
initializeGHCSession :: GHCConfig -> STM (Either GHCError GHCSessionState)
initializeGHCSession config = do
  session <- newGHCSession config
  return $ Right session

-- | Reset session state while preserving interpreter instance
resetGHCSession :: GHCSessionState -> STM ()
resetGHCSession session = cleanupSession session

-- | Get list of currently defined bindings
getSessionBindings :: GHCSessionState -> STM [String]
getSessionBindings = listBindings

-- | Import statement components
data ImportStatement = ImportStatement
  { importModuleName :: String
  , importQualified :: Bool
  , importAlias :: Maybe String
  , importList :: Maybe [String]  -- Nothing = import all, Just [] = import nothing, Just [items] = selective
  } deriving (Show, Eq)

-- | Parse import statement to extract module name and qualification info
parseImportStatement :: String -> (String, Bool)
parseImportStatement stmt = 
  let parsed = parseImportStatementFull stmt
  in (importModuleName parsed, importQualified parsed)

-- | Parse import statement into full structure
parseImportStatementFull :: String -> ImportStatement
parseImportStatementFull stmt = 
  let cleanStmt = dropWhile (== ' ') stmt
      tokens = words cleanStmt
  in case tokens of
    -- import qualified ModuleName
    ["import", "qualified", moduleName] -> 
      ImportStatement moduleName True Nothing Nothing
    -- import qualified ModuleName as Alias
    ["import", "qualified", moduleName, "as", alias] -> 
      ImportStatement moduleName True (Just alias) Nothing
    -- import ModuleName
    ["import", moduleName] -> 
      ImportStatement moduleName False Nothing Nothing
    -- import ModuleName as Alias
    ["import", moduleName, "as", alias] -> 
      ImportStatement moduleName False (Just alias) Nothing
    -- import ModuleName (selective imports)
    ("import":moduleName:rest) | not (null rest) -> 
      let (selective, alias) = parseSelectiveAndAlias (unwords rest)
      in ImportStatement moduleName False alias selective
    -- Just the module name (assume simple import)
    [moduleName] -> 
      ImportStatement moduleName False Nothing Nothing
    -- Default case - treat as module name
    _ -> 
      ImportStatement stmt False Nothing Nothing

-- | Parse selective imports and alias from the rest of import statement
parseSelectiveAndAlias :: String -> (Maybe [String], Maybe String)
parseSelectiveAndAlias rest
  | null rest = (Nothing, Nothing)
  | "(" `isPrefixOf` rest && ")" `isInfixOf` rest = 
      let (selective, remainder) = parseSelectiveImports rest
          alias = parseAliasFromRemainder remainder
      in (selective, alias)
  | " as " `isInfixOf` rest = 
      (Nothing, parseAliasFromRemainder rest)
  | otherwise = (Nothing, Nothing)
  where
    parseSelectiveImports :: String -> (Maybe [String], String)
    parseSelectiveImports str = 
      case break (== ')') str of
        (beforeParen, afterParen) ->
          let itemsStr = drop 1 beforeParen  -- Remove opening '('
              items = map (filter (/= ' ')) $ splitOn ',' itemsStr
              remainder = drop 1 afterParen  -- Remove closing ')'
          in (Just items, remainder)
    
    parseAliasFromRemainder :: String -> Maybe String
    parseAliasFromRemainder str
      | " as " `isInfixOf` str = 
          case words str of
            (_:"as":alias:_) -> Just alias
            _ -> Nothing
      | otherwise = Nothing
    
    splitOn :: Char -> String -> [String]
    splitOn _ "" = []
    splitOn delim str = 
      case break (== delim) str of
        (before, "") -> [before]
        (before, _:after) -> before : splitOn delim after


-- | Parse simple GHCi import module commands into an import statement
-- Examples supported:
--  ":m + Data.List" -> "import Data.List"
--  ":module + Data.List" -> "import Data.List"
parseGHCiImport :: String -> Maybe String
parseGHCiImport s =
  let ws = words s
  in case ws of
    (cmd:flag:moduleName:_) | cmd `elem` [":m", ":module"] && flag `elem` ["+", "+"] ->
      Just $ "import " ++ moduleName
    (cmd:moduleName:_) | cmd `elem` [":m", ":module"] ->
      Just $ "import " ++ moduleName
    _ -> Nothing
    


-- | Validate import statement syntax and security
validateImportStatement :: String -> Either String ()
validateImportStatement stmt
  | null stmt = Left "Empty import statement"
  | length stmt > 200 = Left "Import statement too long"
  | any (`elem` stmt) [';', '&', '|', '`'] = Left "Invalid characters in import statement"
  | otherwise = Right ()

-- | Determines appropriate timeout based on operation complexity
selectTimeout :: GHCSessionState -> EvaluationType -> Text -> Int
selectTimeout session opType code =
  let config = sessionConfig session
      codeStr = T.unpack code
  in case opType of
    Expression -> 
      if isSimpleExpression codeStr 
        then 3  -- 3 seconds for simple expressions (arithmetic, literals)
        else if isComplexExpression codeStr
          then computationTimeout config  -- 30 seconds for complex computations
          else expressionTimeout config   -- 5 seconds for regular expressions
    Declaration ->
      if isSimpleDeclaration codeStr
        then expressionTimeout config     -- 5 seconds for simple let bindings
        else compilationTimeout config    -- 10 seconds for complex declarations
    Import -> compilationTimeout config   -- 10 seconds for imports

-- | Check if expression is simple (arithmetic, literals, basic operations)
isSimpleExpression :: String -> Bool
isSimpleExpression code = 
  let trimmed = filter (not . isSpace) code
      isShort = length code < 50
      isLiteral = all isDigit trimmed || 
                  (length trimmed >= 2 && head trimmed == '"' && last trimmed == '"') ||
                  trimmed `elem` (["True", "False"] :: [String])
      isBasicArithmetic = all (\c -> isDigit c || c `elem` ("+-*/() " :: String)) code
  in isShort && (isLiteral || isBasicArithmetic)

-- | Check if expression involves complex computations (recursion, infinite structures, IO)
isComplexExpression :: String -> Bool  
isComplexExpression code =
  let complexKeywords = ["foldr", "foldl", "iterate", "repeat", "cycle", "fibonacci", 
                        "factorial", "prime", "infinite", "lazy", "unsafePerformIO"]
  in any (`isInfixOf` code) complexKeywords ||
     length code > 200 ||  -- Long expressions likely complex
     countChar '(' code > 5  -- Deep nesting

-- | Check if declaration is simple (basic let binding)
isSimpleDeclaration :: String -> Bool
isSimpleDeclaration code =
  let trimmed = dropWhile isSpace code
  in ("let " `isPrefixOf` trimmed || "=" `elem` words code) &&
     length code < 100 &&
     not (any (`isInfixOf` code) ["where", "case", "if", "do", "data", "newtype", "class"])

-- | Count occurrences of a character in a string
countChar :: Char -> String -> Int
countChar c = length . filter (== c)

-- | Default GHC configuration with differentiated timeouts
defaultGHCConfig :: GHCConfig
defaultGHCConfig = GHCConfig
  { expressionTimeout = 5       -- 5 seconds for regular expressions
  , compilationTimeout = 10     -- 10 seconds for imports/compilation
  , computationTimeout = 30     -- 30 seconds for complex computations
  , importPolicy = defaultSafePolicy
  , resourceLimits = defaultResourceBudget
  }
  where
    -- Safe default import policy - allow safe modules, deny system modules by default
    defaultSafePolicy = ImportPolicy
      { allowedModules = defaultSafeModules
      , deniedModules = mempty  
      , defaultPolicy = Deny
      , systemModulesAllowed = False
      }
    
    -- Default resource budget - generous for development
    defaultResourceBudget = ResourceBudget
      { rbCpuTimeout = 30.0
      , rbMemoryLimit = 256 * 1024 * 1024  -- 256MB
      , rbTempDirectory = "/tmp"
      , rbMaxStreamBytes = 1024 * 1024     -- 1MB
      }