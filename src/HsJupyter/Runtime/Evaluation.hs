{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsJupyter.Runtime.Evaluation
  ( evaluateCell
  ) where

import Control.Concurrent.STM (tryReadTMVar, atomically)
-- MVar not needed here; concurrent coordination uses STM in this module
import Data.Aeson (Value(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import HsJupyter.Runtime.Diagnostics (mkInfo)
import HsJupyter.Runtime.GHCRuntime (evaluateExpression, evaluateDeclaration, importModule, defaultGHCConfig)
import HsJupyter.Runtime.ResourceGuard (truncateOutput)
import HsJupyter.Runtime.GHCSession (newGHCSession, GHCSessionState(..), GHCConfig(..))
import HsJupyter.Runtime.GHCDiagnostics (ghcErrorToDiagnostic)
import System.IO.Unsafe (unsafePerformIO)
import HsJupyter.Runtime.SessionState
  ( ResourceBudget(..)
  , ExecutionJob(..)
  , ExecutionOutcome(..)
  , ExecutionStatus(..)
  , JobType(..)
  , RuntimeSessionState(..)
  , StreamChunk(..)
  , StreamName(..)
  , incrementExecutionCount
  , rssExecutionCount
  )

-- | Global GHC session storage
-- This is a temporary solution for Phase 4. In a production system,
-- this would be managed per-user session with proper cleanup.
{-# NOINLINE globalGHCSession #-}
globalGHCSession :: IORef (Maybe GHCSessionState)
globalGHCSession = unsafePerformIO (newIORef Nothing)

-- | Get or create the persistent GHC session with the given resource budget
-- Note: This recreates the session if the budget changes, which loses state.
-- This is a temporary solution. In production, we should either:
-- 1. Pass budget separately to evaluation functions (without recreating session)
-- 2. Use a TVar for session config to update it without losing state
getOrCreateGHCSession :: ResourceBudget -> IO GHCSessionState
getOrCreateGHCSession budget = do
  maybeSession <- readIORef globalGHCSession
  case maybeSession of
    Just session -> do
      -- Check if the session's budget matches the requested budget
      let currentBudget = resourceLimits (sessionConfig session)
      if currentBudget == budget
        then return session  -- Reuse existing session
        else do
          -- Budget changed, need to recreate session (loses state!)
          let config = defaultGHCConfig { resourceLimits = budget }
          newSession <- atomically $ newGHCSession config
          writeIORef globalGHCSession (Just newSession)
          return newSession
    Nothing -> do
      -- Create new session with the provided budget
      let config = defaultGHCConfig { resourceLimits = budget }
      session <- atomically $ newGHCSession config
      writeIORef globalGHCSession (Just session)
      return session

-- | Determine if code is an import statement
isImport :: Text -> Bool
isImport code = "import " `T.isPrefixOf` T.strip code

-- | Determine if code is a declaration (let, function definition) or expression
isDeclaration :: Text -> Bool
isDeclaration code = 
  let trimmed = T.strip code
      codeStr = T.unpack trimmed
      codeLines = lines codeStr
  in -- Check for let bindings
     "let " `T.isPrefixOf` trimmed ||
     -- Check for function definitions (name followed by parameters and =)
     any isFunctionDefinition codeLines ||
     -- Check for simple assignments (x = ...)
     any isSimpleAssignment codeLines
  where
    isFunctionDefinition line =
      let trimmedLine = dropWhile (== ' ') line
          words' = words trimmedLine
      in case words' of
           (name:_params) -> '=' `elem` line && 
                           not ("==" `isInfixOf` line) && 
                           not ("=>" `isInfixOf` line) &&
                           not (null name) &&
                           all (`notElem` name) ['(', ')', '[', ']', '{', '}', '\'', '"']
           [] -> False
    
    isSimpleAssignment line =
      let trimmedLine = dropWhile (== ' ') line
      in not (null trimmedLine) &&
         '=' `elem` line && 
         not ("==" `isInfixOf` line) && 
         not ("=>" `isInfixOf` line) &&
         not ("import " `isPrefixOf` trimmedLine) &&
         not ("data " `isPrefixOf` trimmedLine) &&
         not ("type " `isPrefixOf` trimmedLine)

-- | Evaluate a cell within the runtime session.
-- Uses the hint library to provide real GHC interpretation.
evaluateCell
  :: RuntimeSessionState
  -> ExecutionJob
  -> IO (ExecutionOutcome, RuntimeSessionState)
evaluateCell state job = do
  startTime <- getCurrentTime
  
  -- Check for cancellation before starting
  cancelled <- atomically $ tryReadTMVar (jobCancelToken job)
  case cancelled of
    Just _ -> do
      endTime <- getCurrentTime
      let duration = diffUTCTime endTime startTime
          outcome = ExecutionOutcome
            { outcomeStatus = ExecutionAbort
            , outcomeStreams = [StreamChunk StreamStderr "Execution cancelled"]
            , outcomePayload = []
            , outcomeDiagnostics = [mkInfo "Execution was cancelled"]
            , outcomeExecutionCount = rssExecutionCount state
            , outcomeDuration = duration
            }
      pure (outcome, state)
    Nothing -> do
      -- Proceed with evaluation
      (result, newState) <- runEvaluation state job
      endTime <- getCurrentTime
      let duration = diffUTCTime endTime startTime
          finalOutcome = result { outcomeDuration = duration }
      pure (finalOutcome, newState)

-- | Run the actual evaluation based on job type
runEvaluation
  :: RuntimeSessionState
  -> ExecutionJob
  -> IO (ExecutionOutcome, RuntimeSessionState)
runEvaluation state job = do
  let newState = incrementExecutionCount state
      executionCount = rssExecutionCount newState
  
  case jobType job of
    EchoJob -> runEchoEvaluation newState executionCount job
    GHCJob -> runGHCEvaluation newState executionCount job

-- | Run echo evaluation (legacy)
runEchoEvaluation
  :: RuntimeSessionState
  -> Int
  -> ExecutionJob
  -> IO (ExecutionOutcome, RuntimeSessionState)
runEchoEvaluation newState executionCount job = do
  -- Apply output truncation based on resource budget
  let outputLimit = fromIntegral $ rbMaxStreamBytes (rssResourceBudget newState)
      sourceText = T.unpack (jobSource job)
      truncated = T.pack $ truncateOutput outputLimit sourceText
      stream = StreamChunk StreamStdout truncated
      outcome = ExecutionOutcome
        { outcomeStatus = ExecutionOk
        , outcomeStreams = [stream]
        , outcomePayload = [String truncated]  -- Use truncated value in payload
        , outcomeDiagnostics = [mkInfo "Echo evaluation (legacy mode)"]
        , outcomeExecutionCount = executionCount
        , outcomeDuration = 0  -- Will be set by caller
        }
  pure (outcome, newState)

-- | Run GHC evaluation using hint library
runGHCEvaluation
  :: RuntimeSessionState
  -> Int
  -> ExecutionJob
  -> IO (ExecutionOutcome, RuntimeSessionState)
runGHCEvaluation newState executionCount job = do
  -- Get or create persistent GHC session with the resource budget from runtime state
  ghcSession <- getOrCreateGHCSession (rssResourceBudget newState)
  
  -- Handle import statements separately
  result <- if isImport (jobSource job)
              then do
                -- Extract the import statement (remove "import " prefix if present)
                let importStmt = T.unpack (jobSource job)
                importResult <- importModule ghcSession importStmt
                case importResult of
                  Left err -> return $ Left err
                  Right () -> return $ Right ""  -- Empty result for successful import
              -- Evaluate the expression (or declaration based on content)
              else if isDeclaration (jobSource job)
                then do
                  declResult <- evaluateDeclaration ghcSession (jobSource job)
                  case declResult of
                    Left err -> return $ Left err
                    Right bindings -> return $ Right (T.pack $ "Defined: " ++ unwords bindings)
                else evaluateExpression ghcSession (jobSource job)
  
  case result of
    Left ghcError -> do
      let diagnostic = ghcErrorToDiagnostic ghcError
          outcome = ExecutionOutcome
            { outcomeStatus = ExecutionError
            , outcomeStreams = []
            , outcomePayload = []
            , outcomeDiagnostics = [diagnostic]
            , outcomeExecutionCount = executionCount
            , outcomeDuration = 0  -- Will be set by caller
            }
      pure (outcome, newState)
    
    Right value -> do
      let stream = StreamChunk StreamStdout value
          outcome = ExecutionOutcome
            { outcomeStatus = ExecutionOk
            , outcomeStreams = [stream]
            , outcomePayload = [String value]  -- Simple String payload for now
            , outcomeDiagnostics = [mkInfo "GHC evaluation successful"]
            , outcomeExecutionCount = executionCount
            , outcomeDuration = 0  -- Will be set by caller
            }
      pure (outcome, newState)