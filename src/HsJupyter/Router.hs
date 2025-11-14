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
import qualified Katip as K

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
  -- In a real implementation, this would query the GHC version, etc.
  let reply = KernelInfoReply
        { languageInfo = LanguageInfo
            { liName = "haskell"
            , liVersion = "9.2.4" -- TODO: Get from GHC API
            , liMimetype = "text/x-haskell"
            , liFileExtension = ".hs"
            }
        , protocolVersion = "5.3"
        , implementation = "hs-jupyter"
        , implementationVersion = "0.1.0" -- TODO: Get from Cabal
        , banner = "HsJupyter - A Haskell kernel for Jupyter"
        }
  return $ replyEnvelope envelope "kernel_info_reply" (Aeson.toJSON reply)

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
      return $ Right [replyEnvelope envelope "execute_reply" (Aeson.toJSON reply)]

handleInterrupt :: RuntimeManager -> ProtocolEnvelope Value -> IO (ProtocolEnvelope Value)
handleInterrupt manager envelope = do
  RM.enqueueInterrupt manager (msgId (envelopeHeader envelope))
  return $ replyEnvelope envelope "interrupt_reply" (Aeson.toJSON $ InterruptReply "ok")

handleShutdown :: RuntimeManager -> ProtocolEnvelope Value -> IO (ProtocolEnvelope Value)
handleShutdown _manager envelope =
  case Aeson.fromJSON (envelopeContent envelope) of
    Aeson.Error _ ->
      -- This shouldn't happen for a valid shutdown request
      return $ replyEnvelope envelope "shutdown_reply" (Aeson.toJSON $ ShutdownReply False)
    Aeson.Success (ShutdownRequest restart) -> do
      -- TODO: Implement actual shutdown logic in RuntimeManager
      -- RM.enqueueShutdown manager restart
      return $ replyEnvelope envelope "shutdown_reply" (Aeson.toJSON $ ShutdownReply restart)

-- | Helper to create a reply envelope.
-- Creates a properly structured reply envelope with signature payload.
-- Fix for: Replaced 'undefined' placeholder with proper signaturePayloadFrom call.
replyEnvelope :: (Aeson.ToJSON a)
              => ProtocolEnvelope any
              -> Text
              -> a
              -> ProtocolEnvelope a
replyEnvelope reqEnv newMsgType content =
  let reqHdr = envelopeHeader reqEnv
      newHdr = MessageHeader
        { msgId = "reply-" <> msgId reqHdr -- TODO: Generate proper UUID
        , msgType = newMsgType
        , session = session reqHdr
        , username = username reqHdr
        , version = version reqHdr
        , date = date reqHdr -- TODO: Update timestamp
        }
      metadata = Aeson.object []
  in ProtocolEnvelope
       { envelopeIdentities = envelopeIdentities reqEnv
       , envelopeHeader = newHdr
       , envelopeParent = Just reqHdr
       , envelopeMetadata = metadata
       , envelopeContent = content
       , envelopeSignature = "" -- The bridge will sign this
       , envelopeSignaturePayload = signaturePayloadFrom newHdr (Just reqHdr) metadata (Aeson.toJSON content)
       }
