{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsJupyter.Runtime.Evaluation
  ( evaluateCell
  ) where

import Control.Concurrent.STM (tryReadTMVar, atomically)
-- MVar not needed here; concurrent coordination uses STM in this module
import Data.Aeson (object, (.=), Value(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import HsJupyter.Runtime.Diagnostics (mkInfo)
import HsJupyter.Runtime.GHCRuntime (evaluateExpression, evaluateDeclaration, defaultGHCConfig)
import HsJupyter.Runtime.GHCSession (newGHCSession, GHCSessionState)
import HsJupyter.Runtime.GHCDiagnostics (ghcErrorToDiagnostic)
import System.IO.Unsafe (unsafePerformIO)
import HsJupyter.Runtime.SessionState
  ( ExecutionJob(..)
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

-- | Get or create the persistent GHC session
getOrCreateGHCSession :: IO GHCSessionState
getOrCreateGHCSession = do
  maybeSession <- readIORef globalGHCSession
  case maybeSession of
    Just session -> return session
    Nothing -> do
      -- Create new session with default config
      session <- atomically $ newGHCSession defaultGHCConfig
      writeIORef globalGHCSession (Just session)
      return session

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
  let payload = object
        [ "execution_count" .= executionCount
        , "data" .= object
            [ "text/plain" .= jobSource job
            ]
        ]
      stream = StreamChunk StreamStdout (jobSource job)
      outcome = ExecutionOutcome
        { outcomeStatus = ExecutionOk
        , outcomeStreams = [stream]
        , outcomePayload = [payload]
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
  -- Get or create persistent GHC session
  ghcSession <- getOrCreateGHCSession
  
  -- Evaluate the expression (or declaration based on content)
  result <- if isDeclaration (jobSource job)
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