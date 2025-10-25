module HsJupyter.Bridge.JupyterBridge
  ( BridgeContext(..)
  , BridgeError(..)
  , mkBridgeContext
  , handleExecuteOnce
  , handleInterrupt
  , logBridgeEvent
  , rejectedCount
  ) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import HsJupyter.Bridge.Protocol.Codec
  ( verifySignature
  )
import HsJupyter.Bridge.Protocol.Envelope
  ( ExecuteReply(..)
  , InterruptReply(..)
  , MessageHeader(..)
  , ProtocolEnvelope(..)
  , emptyMetadata
  , fromExecuteRequest
  , toExecuteReply
  )
import HsJupyter.Kernel.Types
  ( KernelProcessConfig(..)
  , LogLevel(..)
  , summariseConfig
  , shouldLog
  )
import HsJupyter.Router.RequestRouter
  ( Router
  , RuntimeExecutionOutcome(..)
  , RuntimeStreamChunk(..)
  , acknowledgeInterrupt
  , mkRouter
  , routeExecuteRequest
  )
import HsJupyter.Runtime.Manager
  ( RuntimeManager
  )

-- | Operational context shared by bridge handlers.
data BridgeContext = BridgeContext
  { bridgeConfig    :: KernelProcessConfig
  , bridgeRouter    :: Router
  , bridgeManager   :: RuntimeManager
  , bridgeRejected  :: IORef Int
  }

-- | Failures surfaced to callers for error handling/logging.
data BridgeError
  = SignatureValidationFailed
  | DecodeFailure Text
  deriving (Eq, Show)

-- | Construct a bridge context from kernel configuration and runtime manager.
mkBridgeContext :: KernelProcessConfig -> RuntimeManager -> IO BridgeContext
mkBridgeContext cfg manager = do
  rejectedVar <- newIORef 0
  let router = mkRouter manager
  pure BridgeContext
    { bridgeConfig = cfg
    , bridgeRouter = router
    , bridgeManager = manager
    , bridgeRejected = rejectedVar
    }

-- | Report how many envelopes were rejected for signature failures.
rejectedCount :: BridgeContext -> IO Int
rejectedCount = readIORef . bridgeRejected

-- | Process a single execute_request envelope and produce reply + streams.
handleExecuteOnce
  :: BridgeContext
  -> ProtocolEnvelope Value
  -> IO (Either BridgeError [ProtocolEnvelope Value])
handleExecuteOnce ctx envelope = do
  let sharedKey = key (bridgeConfig ctx)
  if not (verifySignature sharedKey envelope)
    then do
      incrementRejected ctx
      logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn "Rejected envelope with invalid signature"
      pure $ Left SignatureValidationFailed
    else case fromExecuteRequest envelope of
      Nothing -> do
        logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn "Failed to decode execute_request"
        pure $ Left (DecodeFailure "Unsupported content type")
      Just typed -> do
        outcome <- routeExecuteRequest (bridgeRouter ctx) typed
        let replyEnv = toExecuteReply typed (toReply outcome)
            streamEnvs = makeStreamEnvelope typed <$> routerStreams outcome
        logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogDebug
          ("Processed message " <> msgId (envelopeHeader typed))
        pure $ Right (replyEnv : streamEnvs)
  where
    toReply outcome = ExecuteReply
      { executeReplyCount = routerCount outcome
      , executeReplyStatus = routerStatus outcome
      , executeReplyPayload = [routerPayload outcome]
      }

    makeStreamEnvelope reqEnv (RuntimeStreamChunk name text) =
      let header = (envelopeHeader reqEnv) { msgType = "stream" }
      in ProtocolEnvelope
          { envelopeIdentities = [T.pack "stream"]
          , envelopeHeader = header
          , envelopeParent = Just (envelopeHeader reqEnv)
          , envelopeMetadata = emptyMetadata
          , envelopeContent = object
              [ "name" .= name
              , "text" .= text
              ]
          , envelopeSignature = ""
          }

-- | Produce an interrupt acknowledgement envelope.
handleInterrupt
  :: BridgeContext
  -> ProtocolEnvelope Value
  -> ProtocolEnvelope Value
handleInterrupt ctx env = env
  { envelopeContent = object ["status" .= status]
  , envelopeHeader = (envelopeHeader env) { msgType = "interrupt_reply" }
  , envelopeSignature = ""
  }
  where
    InterruptReply status = acknowledgeInterrupt (bridgeRouter ctx) env

-- | Lightweight console logger used during development.
logBridgeEvent :: KernelProcessConfig -> LogLevel -> LogLevel -> Text -> IO ()
logBridgeEvent cfg threshold level msg =
  if shouldLog level threshold
    then do
      timestamp <- getCurrentTime
      putStrLn $ T.unpack $ T.concat
        [ "["
        , T.pack (show timestamp)
        , "]"
        , " "
        , T.pack (show level)
        , " "
        , summariseConfig cfg
        , " -- "
        , msg
        ]
    else pure ()

incrementRejected :: BridgeContext -> IO ()
incrementRejected ctx =
  atomicModifyIORef' (bridgeRejected ctx) $ \current -> (current + 1, ())
