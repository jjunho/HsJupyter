module HsJupyter.Runtime.EchoRuntime
  ( EchoRuntime
  , ExecutionOutcome(..)
  , StreamChunk(..)
  , mkRuntime
  , executeRequest
  ) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import HsJupyter.Bridge.Protocol.Envelope (ExecuteRequest(..), ExecuteStatus(..))

-- | Structured stdout/stderr chunk emitted during execution.
data StreamChunk = StreamChunk
  { streamName :: Text
  , streamText :: Text
  } deriving (Eq, Show)

-- | Result of executing a cell via the echo runtime.
data ExecutionOutcome = ExecutionOutcome
  { outcomeStatus  :: ExecuteStatus
  , outcomePayload :: Value
  , outcomeStreams :: [StreamChunk]
  , outcomeCount   :: Int
  } deriving (Eq, Show)

-- | Runtime handle storing execution counter state.
newtype EchoRuntime = EchoRuntime (IORef Int)

-- | Construct a new runtime with counter seeded at zero.
mkRuntime :: IO EchoRuntime
mkRuntime = EchoRuntime <$> newIORef 0

-- | Execute an echo request, returning deterministic payload + streams.
executeRequest :: EchoRuntime -> ExecuteRequest -> IO ExecutionOutcome
executeRequest (EchoRuntime counterRef) req = do
  executionCount <- atomicModifyIORef' counterRef $ \current ->
    let nextVal = current + 1
    in (nextVal, nextVal)
  let payload = object
        [ "source" .= erCode req
        , "silent" .= erSilent req
        ]
      streams =
        [ StreamChunk "stdout" (erCode req)
        ]
  pure ExecutionOutcome
    { outcomeStatus = ExecuteOk
    , outcomePayload = payload
    , outcomeStreams = streams
    , outcomeCount = executionCount
    }
