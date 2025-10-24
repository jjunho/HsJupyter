module HsJupyter.Router.RequestRouter
  ( Router(..)
  , RuntimeExecutionOutcome(..)
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

-- | Request router orchestrating calls into the runtime manager.
newtype Router = Router
  { routerRuntime :: RuntimeManager
  }

-- | Construct a router from a runtime manager handle.
mkRouter :: RuntimeManager -> Router
mkRouter = Router

-- | Route an execute request to the runtime and return the resulting outcome.
routeExecuteRequest :: Router -> ProtocolEnvelope ExecuteRequest -> IO RuntimeExecutionOutcome
routeExecuteRequest (Router manager) env = do
  let req = envelopeContent env
      header = envelopeHeader env
      ctx = ExecuteContext
        { ecMessageId = msgId header
        , ecSessionId = session header
        , ecUsername = username header
        , ecParentId = Nothing  -- Could extract from parent header if needed
        }
      metadata = JobMetadata
        { jmSilent = erSilent req
        , jmStoreHistory = not (erSilent req)  -- Default behavior
        , jmAllowStdin = False  -- Default for now
        , jmUserExpressions = object []  -- Default for now
        }
  outcome <- submitExecute manager ctx metadata (erCode req)
  pure $ convertOutcome outcome

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
  , routerPayload = object
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
acknowledgeInterrupt :: Router -> ProtocolEnvelope Value -> InterruptReply
acknowledgeInterrupt (Router _manager) _env =
  -- Note: We can't perform IO here, so we can't actually enqueue the interrupt.
  -- This would need to be redesigned to support proper cancellation.
  InterruptReply "ok"
