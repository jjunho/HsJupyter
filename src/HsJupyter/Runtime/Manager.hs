{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Runtime.Manager
  ( RuntimeManager(..)
  , withRuntimeManager
  , submitExecute
  , submitGHCExecute
  , enqueueInterrupt
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( STM
  , TBQueue
  , TMVar
  , TVar
  , atomically
  , modifyTVar'
  , newEmptyTMVarIO
  , newTBQueueIO
  , newTVarIO
  , putTMVar
  , readTBQueue
  , readTVar
  , takeTMVar
  , tryPutTMVar
  , tryReadTMVar
  , tryTakeTMVar
  , writeTBQueue
  , writeTVar
  )
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import GHC.Natural (Natural)

import HsJupyter.Runtime.Evaluation (evaluateCell)
import HsJupyter.Runtime.SessionState
  ( ExecuteContext(..)
  , ExecutionJob(..)
  , ExecutionOutcome(..)
  , JobMetadata(..)
  , JobType(..)
  , RuntimeSessionState
  , initialSessionState
  , ResourceBudget(..)
  )

-- | Handle exposed to router/kernel for submitting jobs and interrupts.
data RuntimeManager = RuntimeManager
  { rmSubmit :: ExecuteContext -> JobMetadata -> Text -> IO ExecutionOutcome
  , rmSubmitGHC :: ExecuteContext -> JobMetadata -> Text -> IO ExecutionOutcome
  , rmInterrupt :: Text -> IO ()
  }

submitExecute :: RuntimeManager -> ExecuteContext -> JobMetadata -> Text -> IO ExecutionOutcome
submitExecute mgr = rmSubmit mgr

submitGHCExecute :: RuntimeManager -> ExecuteContext -> JobMetadata -> Text -> IO ExecutionOutcome
submitGHCExecute mgr = rmSubmitGHC mgr

enqueueInterrupt :: RuntimeManager -> Text -> IO ()
enqueueInterrupt mgr = rmInterrupt mgr

withRuntimeManager
  :: ResourceBudget
  -> Natural -- ^ queue capacity
  -> (RuntimeManager -> IO a)
  -> IO a
withRuntimeManager budget capacity action = do
  queue <- newTBQueueIO capacity
  cancelMap <- newTVarIO Map.empty
  stateVar <- newTVarIO (initialSessionState budget)
  bracket (spawnWorker queue cancelMap stateVar)
          cancel
          (const (action (manager queue cancelMap stateVar)))

manager
  :: TBQueue (ExecutionJob, TMVar ExecutionOutcome)
  -> TVar (Map Text (TMVar ()))
  -> TVar RuntimeSessionState
  -> RuntimeManager
manager queue cancelMap _stateVar =
  RuntimeManager
    { rmSubmit = submit queue cancelMap
    , rmSubmitGHC = submitGHCExecuteInternal queue cancelMap
    , rmInterrupt = cancelJob cancelMap
    }

spawnWorker
  :: TBQueue (ExecutionJob, TMVar ExecutionOutcome)
  -> TVar (Map Text (TMVar ()))
  -> TVar RuntimeSessionState
  -> IO (Async ())
spawnWorker queue cancelMap stateVar = async (workerLoop queue cancelMap stateVar)

submit
  :: TBQueue (ExecutionJob, TMVar ExecutionOutcome)
  -> TVar (Map Text (TMVar ()))
  -> ExecuteContext
  -> JobMetadata
  -> Text
  -> IO ExecutionOutcome
submit queue cancelMap ctx metadata source = do
  cancelToken <- newEmptyTMVarIO
  replyVar <- newEmptyTMVarIO
  submittedAt <- getCurrentTime
  let job = ExecutionJob
        { jobContext = ctx
        , jobSource = source
        , jobSubmittedAt = submittedAt
        , jobMetadata = metadata
        , jobCancelToken = cancelToken
        , jobType = EchoJob  -- Default to EchoJob for backward compatibility, GHC support added later
        }
  atomically $ do
    modifyTVar' cancelMap (Map.insert (ecMessageId ctx) cancelToken)
    writeTBQueue queue (job, replyVar)
  atomically (takeTMVar replyVar)

-- | Internal GHC submit function
submitGHCExecuteInternal
  :: TBQueue (ExecutionJob, TMVar ExecutionOutcome)
  -> TVar (Map Text (TMVar ()))
  -> ExecuteContext
  -> JobMetadata
  -> Text
  -> IO ExecutionOutcome
submitGHCExecuteInternal queue cancelMap ctx metadata source = do
  cancelToken <- newEmptyTMVarIO
  replyVar <- newEmptyTMVarIO
  submittedAt <- getCurrentTime
  let job = ExecutionJob
        { jobContext = ctx
        , jobSource = source
        , jobSubmittedAt = submittedAt
        , jobMetadata = metadata
        , jobCancelToken = cancelToken
        , jobType = GHCJob  -- Use GHC evaluation
        }
  atomically $ do
    modifyTVar' cancelMap (Map.insert (ecMessageId ctx) cancelToken)
    writeTBQueue queue (job, replyVar)
  atomically (takeTMVar replyVar)

cancelJob :: TVar (Map Text (TMVar ())) -> Text -> IO ()
cancelJob cancelMap msgId = atomically $ do
  table <- readTVar cancelMap
  case Map.lookup msgId table of
    Nothing     -> pure ()
    Just token  -> do
      void $ tryPutTMVar token ()
      pure ()

workerLoop
  :: TBQueue (ExecutionJob, TMVar ExecutionOutcome)
  -> TVar (Map Text (TMVar ()))
  -> TVar RuntimeSessionState
  -> IO ()
workerLoop queue cancelMap stateVar = forever $ do
  (job, replyVar) <- atomically (readTBQueue queue)
  state <- atomically (readTVar stateVar)
  (outcome, newState) <- evaluateCell state job
  atomically $ do
    writeTVar stateVar newState
    putTMVar replyVar outcome
    modifyTVar' cancelMap (Map.delete (ecMessageId (jobContext job)))
