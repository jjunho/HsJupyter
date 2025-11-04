{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Bridge.JupyterBridge
  ( BridgeContext(..)
  , BridgeError(..)
  , mkBridgeContext
  , handleExecuteOnce
  , handleKernelInfo
  , handleInterrupt
  , logBridgeEvent
  , rejectedCount
  , incrementRejected
  , statusEnvelope
  ) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import System.Info (compilerVersion)

import HsJupyter.Bridge.Protocol.Codec (verifySignature)
import HsJupyter.Bridge.Protocol.Envelope
  ( ExecuteReply(..)
  , ExecuteRequest(..)
  , ExecuteStatus(..)
  , InterruptReply(..)
  , KernelInfoReply(..)
  , MessageHeader(..)
  , ProtocolEnvelope(..)
  , emptyMetadata
  , envelopeContent
  , envelopeHeader
  , envelopeIdentities
  , envelopeParent
  , fromKernelInfoRequest
  , msgType
  , signaturePayloadFrom
  , toExecuteReply
  , toKernelInfoReply
  )
import HsJupyter.Kernel.Types
  ( KernelProcessConfig(..)
  , LogLevel(..)
  , summariseConfig
  , shouldLog
  )
import HsJupyter.Router.RequestRouter
  ( Router
  , acknowledgeInterrupt
  , mkRouter
  , routeExecuteRequest
  )
import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic(..))
import HsJupyter.Runtime.Manager
  ( RuntimeManager
  )
import HsJupyter.Runtime.SessionState
  ( ExecutionOutcome(..)
  , ExecutionStatus(..)
  , StreamChunk(..)
  , StreamName(..)
  )

-- | Operational context shared by bridge handlers.
data BridgeContext = BridgeContext
  { bridgeConfig   :: KernelProcessConfig
  , bridgeRouter   :: Router
  , bridgeManager  :: RuntimeManager
  , bridgeRejected :: IORef Int
  }

-- | Failures surfaced to callers for error handling/logging.
data BridgeError
  = SignatureValidationFailed
  | DecodeFailure Text
  deriving (Eq, Show)

mkBridgeContext :: KernelProcessConfig -> RuntimeManager -> IO BridgeContext
mkBridgeContext cfg manager = do
  rejectedVar <- newIORef 0
  pure BridgeContext
    { bridgeConfig = cfg
    , bridgeRouter = mkRouter manager
    , bridgeManager = manager
    , bridgeRejected = rejectedVar
    }

rejectedCount :: BridgeContext -> IO Int
rejectedCount = readIORef . bridgeRejected

handleExecuteOnce
  :: BridgeContext
  -> ProtocolEnvelope ExecuteRequest
  -> IO [ProtocolEnvelope Value]
handleExecuteOnce ctx request = do
  outcome <- routeExecuteRequest (bridgeRouter ctx) request
  pure (outcomeEnvelopes request outcome)

-- | Handle kernel_info_request and return kernel_info_reply
handleKernelInfo
  :: BridgeContext
  -> ProtocolEnvelope Value
  -> IO (Either BridgeError (ProtocolEnvelope Value))
handleKernelInfo ctx envelope = do
  let sharedKey = key (bridgeConfig ctx)
  if not (verifySignature sharedKey envelope)
    then do
      incrementRejected ctx
      logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn "Rejected kernel_info_request with invalid signature"
      pure $ Left SignatureValidationFailed
    else case fromKernelInfoRequest envelope of
      Nothing -> do
        logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn "Failed to decode kernel_info_request"
        pure $ Left (DecodeFailure "Not a kernel_info_request")
      Just typed -> do
        newMsgId <- UUID.toText <$> UUID.nextRandom
        let compilerVer = T.pack (showVersion compilerVersion)
        let reply = KernelInfoReply
              { kirProtocolVersion = "5.3"
              , kirImplementation = "hsjupyter"
              , kirImplementationVersion = "0.1.0"
              , kirLanguageInfo = object
                  [ "name" .= ("haskell" :: Text)
                  , "version" .= compilerVer
                  , "mimetype" .= ("text/x-haskell" :: Text)
                  , "file_extension" .= (".hs" :: Text)
                  , "pygments_lexer" .= ("haskell" :: Text)
                  , "codemirror_mode" .= ("haskell" :: Text)
                  ]
              , kirBanner = "HsJupyter - Haskell kernel for Jupyter (GHC " <> compilerVer <> ")"
              , kirHelpLinks = []
              , kirStatus = "ok"
              }
            replyEnv = toKernelInfoReply typed reply
            updatedHeader = (envelopeHeader replyEnv) { msgId = newMsgId }
            updatedEnv = replyEnv
              { envelopeHeader = updatedHeader
              , envelopeSignaturePayload = signaturePayloadFrom
                  updatedHeader
                  (envelopeParent replyEnv)
                  (envelopeMetadata replyEnv)
                  (envelopeContent replyEnv)
              }
        pure $ Right updatedEnv

-- | Produce an interrupt acknowledgement envelope.
handleInterrupt
  :: BridgeContext
  -> ProtocolEnvelope Value
  -> IO (ProtocolEnvelope Value)
handleInterrupt ctx env = do
  InterruptReply status <- acknowledgeInterrupt (bridgeRouter ctx) env
  let replyHeader = (envelopeHeader env) { msgType = "interrupt_reply" }
      parentHeader = Just (envelopeHeader env)
      metadata = emptyMetadata
      replyContent = object ["status" .= status]
  pure env
    { envelopeHeader = replyHeader
    , envelopeParent = parentHeader
    , envelopeMetadata = metadata
    , envelopeContent = replyContent
    , envelopeSignature = ""
    , envelopeSignaturePayload = signaturePayloadFrom replyHeader parentHeader metadata replyContent
    }

-- Envelope rendering -------------------------------------------------------

outcomeEnvelopes
  :: ProtocolEnvelope ExecuteRequest
  -> ExecutionOutcome
  -> [ProtocolEnvelope Value]
outcomeEnvelopes request outcome =
  let reply = toExecuteReply request ExecuteReply
        { executeReplyCount = outcomeExecutionCount outcome
        , executeReplyStatus = statusToReply (outcomeStatus outcome)
        , executeReplyPayload = []
        }
      executeInput = executeInputEnvelope request (outcomeExecutionCount outcome)
      streams = fmap (streamEnvelope request) (outcomeStreams outcome)
      results = zipWith (resultEnvelope request (outcomeExecutionCount outcome)) [0 :: Int ..] (outcomePayload outcome)
      diagnostics = concatMap (diagnosticEnvelopes request) (outcomeDiagnostics outcome)
      idle = statusEnvelope request "idle"
  in reply : executeInput : (streams ++ results ++ diagnostics) ++ [idle]

streamEnvelope
  :: ProtocolEnvelope ExecuteRequest
  -> StreamChunk
  -> ProtocolEnvelope Value
streamEnvelope request (StreamChunk name text) =
  let header = (envelopeHeader request) { msgType = "stream" }
      parentHeader = Just (envelopeHeader request)
      metadata = emptyMetadata
      content = object
        [ "name" .= streamNameLabel name
        , "text" .= text
        ]
  in ProtocolEnvelope
       { envelopeIdentities = []
       , envelopeHeader = header
       , envelopeParent = parentHeader
       , envelopeMetadata = metadata
       , envelopeContent = content
       , envelopeSignature = ""
       , envelopeSignaturePayload = signaturePayloadFrom header parentHeader metadata content
       }

resultEnvelope
  :: ProtocolEnvelope ExecuteRequest
  -> Int
  -> Int
  -> Value
  -> ProtocolEnvelope Value
resultEnvelope request count index value =
  let header = (envelopeHeader request) { msgType = "execute_result" }
      parentHeader = Just (envelopeHeader request)
      content = object
        [ "data" .= value
        , "metadata" .= object []
        , "execution_count" .= count
        , "index" .= index
        ]
      metadata = object []
  in ProtocolEnvelope
       { envelopeIdentities = []
       , envelopeHeader = header
       , envelopeParent = parentHeader
       , envelopeMetadata = metadata
       , envelopeContent = content
       , envelopeSignature = ""
       , envelopeSignaturePayload = signaturePayloadFrom header parentHeader metadata content
       }

executeInputEnvelope
  :: ProtocolEnvelope ExecuteRequest
  -> Int
  -> ProtocolEnvelope Value
executeInputEnvelope request count =
  let header = (envelopeHeader request) { msgType = "execute_input" }
      parentHeader = Just (envelopeHeader request)
      content = object
        [ "code" .= erCode (envelopeContent request)
        , "execution_count" .= count
        ]
      metadata = emptyMetadata
  in ProtocolEnvelope
       { envelopeIdentities = []
       , envelopeHeader = header
       , envelopeParent = parentHeader
       , envelopeMetadata = metadata
       , envelopeContent = content
       , envelopeSignature = ""
       , envelopeSignaturePayload = signaturePayloadFrom header parentHeader metadata content
       }

statusEnvelope
  :: ProtocolEnvelope a
  -> Text
  -> ProtocolEnvelope Value
statusEnvelope request state =
  let header = (envelopeHeader request) { msgType = "status" }
      parentHeader = Just (envelopeHeader request)
      metadata = emptyMetadata
      content = object ["execution_state" .= state]
  in ProtocolEnvelope
       { envelopeIdentities = []
       , envelopeHeader = header
       , envelopeParent = parentHeader
       , envelopeMetadata = metadata
       , envelopeContent = content
       , envelopeSignature = ""
       , envelopeSignaturePayload = signaturePayloadFrom header parentHeader metadata content
       }

diagnosticEnvelopes
  :: ProtocolEnvelope ExecuteRequest
  -> RuntimeDiagnostic
  -> [ProtocolEnvelope Value]
diagnosticEnvelopes request diag =
  let header = (envelopeHeader request) { msgType = "stream" }
      parentHeader = Just (envelopeHeader request)
      metadata = emptyMetadata
      payload = object
        [ "name" .= ("stderr" :: Text)
        , "text" .= renderDiagnostic diag
        ]
  in [ ProtocolEnvelope
         { envelopeIdentities = []
         , envelopeHeader = header
         , envelopeParent = parentHeader
         , envelopeMetadata = metadata
         , envelopeContent = payload
         , envelopeSignature = ""
         , envelopeSignaturePayload = signaturePayloadFrom header parentHeader metadata payload
         }
     ]

renderDiagnostic :: RuntimeDiagnostic -> Text
renderDiagnostic diag =
  let detail = maybe "" (\d -> "\n" <> d) (rdDetail diag)
  in rdSummary diag <> detail

statusToReply :: ExecutionStatus -> ExecuteStatus
statusToReply ExecutionOk            = ExecuteOk
statusToReply ExecutionError         = ExecuteError
statusToReply ExecutionAbort         = ExecuteError
statusToReply ExecutionResourceLimit = ExecuteError

streamNameLabel :: StreamName -> Text
streamNameLabel StreamStdout = "stdout"
streamNameLabel StreamStderr = "stderr"

-- Logging ------------------------------------------------------------------

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
