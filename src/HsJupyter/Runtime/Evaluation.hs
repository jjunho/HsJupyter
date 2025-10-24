{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsJupyter.Runtime.Evaluation
  ( evaluateCell
  ) where

import Control.Concurrent.STM (tryReadTMVar, atomically)
import Data.Aeson (object, (.=), Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)

import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic, mkError, mkInfo)
import HsJupyter.Runtime.GHCRuntime (evaluateExpression, defaultGHCConfig)
import HsJupyter.Runtime.GHCSession (newGHCSession)
import HsJupyter.Runtime.GHCDiagnostics (ghcErrorToDiagnostic)
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
  -- Create a GHC session for evaluation
  ghcSession <- atomically $ newGHCSession defaultGHCConfig
  
  -- Evaluate the expression
  result <- evaluateExpression ghcSession (jobSource job)
  
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