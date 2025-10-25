{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Router.RequestRouter
  ( Router(..)
  , RuntimeStreamChunk(..)
  , mkRouter
  , routeExecuteRequest
  , acknowledgeInterrupt
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import HsJupyter.Bridge.Protocol.Envelope
  ( ExecuteRequest(..)
  , ExecuteStatus(..)
  , InterruptReply(..)
  , MessageHeader(..)
  , ProtocolEnvelope(..)
  )

import HsJupyter.Runtime.Manager
  ( RuntimeManager
  , enqueueInterrupt
  , submitExecute
  )
import HsJupyter.Runtime.SessionState
  ( ExecuteContext(..)
  , ExecutionOutcome(..)
  , ExecutionStatus(..)
  , JobMetadata(..)
  , StreamChunk(..)
  , StreamName(..)
  )

newtype Router = Router
  { routerManager :: RuntimeManager
  }

mkRouter :: RuntimeManager -> Router
mkRouter = Router

routeExecuteRequest
  :: Router
  -> ProtocolEnvelope ExecuteRequest
  -> IO ExecutionOutcome
routeExecuteRequest (Router manager) env = do
  let req = envelopeContent env
      header = envelopeHeader env
      ctx = ExecuteContext
        { ecMessageId = msgId header
        , ecSessionId = session header
        , ecUsername  = username header
        , ecParentId  = msgId <$> envelopeParent env
        }
      metadata = JobMetadata
        { jmSilent = erSilent req
        , jmStoreHistory = erStoreHistory req
        , jmAllowStdin = erAllowStdin req
        , jmUserExpressions = envelopeMetadata env
        }
  submitExecute manager ctx metadata (erCode req)

-- | Convert runtime execution outcome to router-expected format
data RuntimeExecutionOutcome = RuntimeExecutionOutcome
  { routerStatus  :: ExecuteStatus
  , routerPayload :: Value
  , routerStreams :: [RuntimeStreamChunk]
  , routerCount   :: Int
  } deriving (Eq, Show)

data RuntimeStreamChunk = RuntimeStreamChunk
  { chunkName :: Text
  , chunkText :: Text
  } deriving (Eq, Show)

convertOutcome :: ExecutionOutcome -> RuntimeExecutionOutcome
convertOutcome outcome = RuntimeExecutionOutcome
  { routerStatus = convertStatus (outcomeStatus outcome)
  , routerPayload = case outcomePayload outcome of
      (payload:_) -> payload  -- Take first payload if available
      [] -> object
          [ "execution_count" .= outcomeExecutionCount outcome
          , "status" .= statusText (outcomeStatus outcome)
          ]
  , routerStreams = map convertStream (outcomeStreams outcome)
  , routerCount = outcomeExecutionCount outcome
  }
  where
    convertStatus ExecutionOk = ExecuteOk
    convertStatus ExecutionError = ExecuteError
    convertStatus ExecutionAbort = ExecuteError  -- Map abort to error since ExecuteAbort doesn't exist
    convertStatus ExecutionResourceLimit = ExecuteError

    convertStream (StreamChunk name text) = RuntimeStreamChunk (streamNameToText name) text
    
    streamNameToText name = case name of
      StreamStdout -> "stdout"
      StreamStderr -> "stderr"
    
    statusText ExecutionOk = "ok" :: Text
    statusText ExecutionError = "error"
    statusText ExecutionAbort = "abort"
    statusText ExecutionResourceLimit = "error"

-- | Produce an interrupt acknowledgement payload.
acknowledgeInterrupt
  :: Router
  -> ProtocolEnvelope Value
  -> IO InterruptReply
acknowledgeInterrupt (Router manager) env = do
  enqueueInterrupt manager (msgId (envelopeHeader env))
  pure (InterruptReply "ok")
