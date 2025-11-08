{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HsJupyter.Bridge.JupyterBridge
Description : High-level Jupyter protocol bridge handlers
Copyright   : (c) HsJupyter Contributors 2024-2025
License     : MIT
Maintainer  : dev@hsjupyter.org
Stability   : stable

This module provides the bridge between ZeroMQ protocol messages and
the runtime execution system. It handles message validation, routing,
and response generation for all Jupyter kernel protocol message types.

Key responsibilities:
- HMAC signature verification for incoming messages
- Routing execute requests to the runtime manager
- Generating protocol-compliant replies
- Maintaining bridge-level telemetry (rejected message count)
-}

module HsJupyter.Bridge.JupyterBridge
  ( BridgeContext(..)
  , BridgeError(..)
  , mkBridgeContext
  , handleExecuteOnce
  , handleKernelInfo
  , handleInterrupt
  , logBridgeEvent
  , rejectedCount
  ) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Time.Clock (getCurrentTime)

import HsJupyter.Bridge.Protocol.Codec (verifySignature)
import HsJupyter.Bridge.Protocol.Envelope
  ( ExecuteReply(..)
  , ExecuteRequest
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
  , fromExecuteRequest
  , fromKernelInfoRequest
  , msgType
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

-- | Verify signature and handle rejection if invalid (DRY helper)
withSignatureCheck
  :: BridgeContext
  -> ProtocolEnvelope Value
  -> Text
  -> IO (Either BridgeError ())
withSignatureCheck ctx envelope errorMsg = do
  let sharedKey = key (bridgeConfig ctx)
  if not (verifySignature sharedKey envelope)
    then do
      incrementRejected ctx
      logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn errorMsg
      pure $ Left SignatureValidationFailed
    else pure $ Right ()

handleExecuteOnce
  :: BridgeContext
  -> ProtocolEnvelope Value
  -> IO (Either BridgeError [ProtocolEnvelope Value])
handleExecuteOnce ctx envelope = do
  result <- withSignatureCheck ctx envelope "Rejected envelope with invalid signature"
  case result of
    Left err -> pure $ Left err
    Right () -> case fromExecuteRequest envelope of
      Nothing -> do
        logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn "Failed to decode execute_request"
        pure $ Left (DecodeFailure "Unsupported content type")
      Just typed -> do
        outcome <- routeExecuteRequest (bridgeRouter ctx) typed
        pure (Right (outcomeEnvelopes typed outcome))

-- | Handle kernel_info_request and return kernel_info_reply
handleKernelInfo
  :: BridgeContext
  -> ProtocolEnvelope Value
  -> IO (Either BridgeError (ProtocolEnvelope Value))
handleKernelInfo ctx envelope = do
  result <- withSignatureCheck ctx envelope "Rejected kernel_info_request with invalid signature"
  case result of
    Left err -> pure $ Left err
    Right () -> case fromKernelInfoRequest envelope of
      Nothing -> do
        logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn "Failed to decode kernel_info_request"
        pure $ Left (DecodeFailure "Not a kernel_info_request")
      Just typed -> do
        newMsgId <- UUID.toText <$> UUID.nextRandom
        let reply = KernelInfoReply
              { kirProtocolVersion = "5.3"
              , kirImplementation = "hsjupyter"
              , kirImplementationVersion = "0.1.0"
              , kirLanguageInfo = object
                  [ "name" .= ("haskell" :: Text)
                  , "version" .= ("9.12.2" :: Text)
                  , "mimetype" .= ("text/x-haskell" :: Text)
                  , "file_extension" .= (".hs" :: Text)
                  , "pygments_lexer" .= ("haskell" :: Text)
                  , "codemirror_mode" .= ("haskell" :: Text)
                  ]
              , kirBanner = "HsJupyter - Haskell kernel for Jupyter (GHC 9.12.2)"
              , kirHelpLinks = []
              , kirStatus = "ok"
              }
            replyEnv = toKernelInfoReply typed reply
            updatedHeader = (envelopeHeader replyEnv) { msgId = newMsgId }
        pure $ Right (replyEnv { envelopeHeader = updatedHeader })

-- | Produce an interrupt acknowledgement envelope.
handleInterrupt
  :: BridgeContext
  -> ProtocolEnvelope Value
  -> IO (ProtocolEnvelope Value)
handleInterrupt ctx env = do
  InterruptReply status <- acknowledgeInterrupt (bridgeRouter ctx) env
  let replyHeader = (envelopeHeader env) { msgType = "interrupt_reply" }
  pure env
    { envelopeHeader = replyHeader
    , envelopeContent = object ["status" .= status]
    , envelopeSignature = ""
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
      streams = fmap (streamEnvelope request) (outcomeStreams outcome)
      results = zipWith (resultEnvelope request (outcomeExecutionCount outcome)) [0 :: Int ..] (outcomePayload outcome)
      diagnostics = concatMap (diagnosticEnvelopes request) (outcomeDiagnostics outcome)
  in reply : streams ++ results ++ diagnostics

streamEnvelope
  :: ProtocolEnvelope ExecuteRequest
  -> StreamChunk
  -> ProtocolEnvelope Value
streamEnvelope request (StreamChunk name text) =
  let header = (envelopeHeader request) { msgType = "stream" }
      content = object
        [ "name" .= streamNameLabel name
        , "text" .= text
        ]
  in ProtocolEnvelope
       { envelopeIdentities = envelopeIdentities request
       , envelopeHeader = header
       , envelopeParent = Just (envelopeHeader request)
       , envelopeMetadata = emptyMetadata
       , envelopeContent = content
       , envelopeSignature = ""
       }

resultEnvelope
  :: ProtocolEnvelope ExecuteRequest
  -> Int
  -> Int
  -> Value
  -> ProtocolEnvelope Value
resultEnvelope request count index value =
  let header = (envelopeHeader request) { msgType = "execute_result" }
      content = object
        [ "data" .= value
        , "metadata" .= object []
        , "execution_count" .= count
        , "index" .= index
        ]
  in ProtocolEnvelope
       { envelopeIdentities = envelopeIdentities request
       , envelopeHeader = header
       , envelopeParent = Just (envelopeHeader request)
       , envelopeMetadata = object []
       , envelopeContent = content
       , envelopeSignature = ""
       }

diagnosticEnvelopes
  :: ProtocolEnvelope ExecuteRequest
  -> RuntimeDiagnostic
  -> [ProtocolEnvelope Value]
diagnosticEnvelopes request diag =
  let header = (envelopeHeader request) { msgType = "stream" }
      payload = object
        [ "name" .= ("stderr" :: Text)
        , "text" .= renderDiagnostic diag
        ]
  in [ ProtocolEnvelope
         { envelopeIdentities = envelopeIdentities request
         , envelopeHeader = header
         , envelopeParent = Just (envelopeHeader request)
         , envelopeMetadata = emptyMetadata
         , envelopeContent = payload
         , envelopeSignature = ""
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
