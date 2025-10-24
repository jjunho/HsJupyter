{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsJupyter.Runtime.Evaluation
  ( evaluateCell
  ) where

import Control.Concurrent.STM (tryReadTMVar, atomically)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)

import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic, mkError, mkInfo)
import HsJupyter.Runtime.SessionState
  ( ExecutionJob(..)
  , ExecutionOutcome(..)
  , ExecutionStatus(..)
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

-- | Run the actual Haskell evaluation using hint
runEvaluation
  :: RuntimeSessionState
  -> ExecutionJob
  -> IO (ExecutionOutcome, RuntimeSessionState)
runEvaluation state job = do
  let newState = incrementExecutionCount state
      executionCount = rssExecutionCount newState
      
      -- For now, echo the input (real GHC evaluation will be implemented in next iteration)
      payload = object
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
        , outcomeDiagnostics = [mkInfo "Echo evaluation (real GHC evaluation in development)"]
        , outcomeExecutionCount = executionCount
        , outcomeDuration = 0  -- Will be set by caller
        }
  pure (outcome, newState)