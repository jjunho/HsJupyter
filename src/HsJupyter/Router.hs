{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsJupyter.Router
  ( routeRequest
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Data.Version (showVersion)
import qualified Katip as K
import qualified Language.Haskell.Interpreter as Hint

import           HsJupyter.Bridge.Types (BridgeError (..))
import           HsJupyter.Bridge.Protocol.Envelope (ExecuteReply (..),
                                                  ExecuteRequest (..),
                                                  InterruptReply (..),
                                                  KernelInfoReply (..),
                                                  LanguageInfo (..),
                                                  ProtocolEnvelope (..),
                                                  MessageHeader (..),
                                                  ShutdownReply (..),
                                                  ShutdownRequest (..),
                                                  signaturePayloadFrom)
import           HsJupyter.Runtime.Manager (RuntimeManager)
import qualified HsJupyter.Runtime.Manager as RM
import           HsJupyter.Runtime.SessionState
import qualified Paths_hs_jupyter_kernel as Paths

-- | Main entry point for routing messages from the shell and control channels.
routeRequest :: (K.KatipContext m)
             => RuntimeManager
             -> ProtocolEnvelope Value
             -> m (Either BridgeError [ProtocolEnvelope Value])
routeRequest manager envelope =
  let hdr = envelopeHeader envelope
      mtype = msgType hdr
  in case mtype of
       "kernel_info_request" -> liftIO $ Right . (:[]) <$> handleKernelInfo manager envelope
       "execute_request"     -> handleExecuteRequest manager envelope
       "interrupt_request"   -> liftIO $ Right . (:[]) <$> handleInterrupt manager envelope
       "shutdown_request"    -> liftIO $ Right . (:[]) <$> handleShutdown manager envelope
       _                     -> return $ Left (DecodeFailure ("Unsupported message type: " <> mtype))

handleKernelInfo :: RuntimeManager -> ProtocolEnvelope Value -> IO (ProtocolEnvelope Value)
handleKernelInfo _ envelope = do
  -- Get GHC version - we query the interpreter to ensure it's available
  ghcVersionResult <- Hint.runInterpreter $ do
    Hint.set [Hint.languageExtensions Hint.:= []]
    Hint.eval "System.Info.compilerVersion" :: Hint.Interpreter String

  let ghcVer = case ghcVersionResult of
        Left _  -> "GHC (version unknown)" -- Fallback if GHC not available
        Right v -> "GHC " <> T.pack v

  -- Get implementation version from Cabal-generated Paths module
  let implVersion = T.pack (showVersion Paths.version)

  let reply = KernelInfoReply
        { languageInfo = LanguageInfo
            { liName = "haskell"
            , liVersion = ghcVer
            , liMimetype = "text/x-haskell"
            , liFileExtension = ".hs"
            }
        , protocolVersion = "5.3"
        , implementation = "hs-jupyter"
        , implementationVersion = implVersion
        , banner = "HsJupyter - A Haskell kernel for Jupyter\n" <> ghcVer <> " | Version: " <> implVersion
        }
  replyEnvelope envelope "kernel_info_reply" (Aeson.toJSON reply)

handleExecuteRequest :: (K.KatipContext m)
                     => RuntimeManager
                     -> ProtocolEnvelope Value
                     -> m (Either BridgeError [ProtocolEnvelope Value])
handleExecuteRequest manager envelope =
  case Aeson.fromJSON (envelopeContent envelope) of
    Aeson.Error err -> return $ Left (DecodeFailure (T.pack err))
    Aeson.Success (req :: ExecuteRequest) -> do
      let ctx = ExecuteContext
            { ecMessageId = msgId (envelopeHeader envelope)
            , ecSessionId = session (envelopeHeader envelope)
            , ecUsername  = username (envelopeHeader envelope)
            , ecParentId  = msgId <$> envelopeParent envelope
            }
          metadata = JobMetadata
            { jmSilent = erSilent req
            , jmStoreHistory = erStoreHistory req
            , jmAllowStdin = erAllowStdin req
            , jmUserExpressions = erUserExpressions req
            }
      -- This is where the async execution happens.
      -- The `submitGHCExecute` function will return immediately, and the
      -- results will be sent back on the IOPub socket by the runtime manager.
      liftIO $ RM.submitGHCExecute manager ctx metadata (erCode req)
      -- The shell socket gets an immediate execute_reply
      let reply = ExecuteReply
            { erStatus = "ok"
            , erExecutionCount = -1 -- The runtime will send the real count
            , erPayload = []
            , erReplyUserExpressions = Aeson.object []
            }
      replyEnv <- liftIO $ replyEnvelope envelope "execute_reply" (Aeson.toJSON reply)
      return $ Right [replyEnv]

handleInterrupt :: RuntimeManager -> ProtocolEnvelope Value -> IO (ProtocolEnvelope Value)
handleInterrupt manager envelope = do
  RM.enqueueInterrupt manager (msgId (envelopeHeader envelope))
  replyEnvelope envelope "interrupt_reply" (Aeson.toJSON $ InterruptReply "ok")

handleShutdown :: RuntimeManager -> ProtocolEnvelope Value -> IO (ProtocolEnvelope Value)
handleShutdown _manager envelope =
  case Aeson.fromJSON (envelopeContent envelope) of
    Aeson.Error _ ->
      -- This shouldn't happen for a valid shutdown request
      replyEnvelope envelope "shutdown_reply" (Aeson.toJSON $ ShutdownReply False)
    Aeson.Success (ShutdownRequest restart) -> do
      -- Acknowledge shutdown request. Actual termination is handled by Jupyter.
      -- The kernel process will be terminated by SIGTERM from Jupyter.
      replyEnvelope envelope "shutdown_reply" (Aeson.toJSON $ ShutdownReply restart)

-- | Helper to create a reply envelope.
-- Creates a properly structured reply envelope with signature payload, proper UUID, and current timestamp.
-- Fixed: Generate proper UUID for message ID and update timestamp to current time.
replyEnvelope :: (Aeson.ToJSON a)
              => ProtocolEnvelope any
              -> Text
              -> a
              -> IO (ProtocolEnvelope a)
replyEnvelope reqEnv newMsgType content = do
  -- Generate proper UUID for message ID
  msgUuid <- UUID.nextRandom
  currentTime <- getCurrentTime

  let reqHdr = envelopeHeader reqEnv
      newHdr = MessageHeader
        { msgId = UUID.toText msgUuid
        , msgType = newMsgType
        , session = session reqHdr
        , username = username reqHdr
        , version = version reqHdr
        , date = Just currentTime
        }
      metadata = Aeson.object []

  return ProtocolEnvelope
       { envelopeIdentities = envelopeIdentities reqEnv
       , envelopeHeader = newHdr
       , envelopeParent = Just reqHdr
       , envelopeMetadata = metadata
       , envelopeContent = content
       , envelopeSignature = "" -- The bridge will sign this
       , envelopeSignaturePayload = signaturePayloadFrom newHdr (Just reqHdr) metadata (Aeson.toJSON content)
       }
