{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsJupyter.Router.RequestRouter
  ( routeRequest
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Katip

import           HsJupyter.Bridge.JupyterBridge (BridgeError (..))
import           HsJupyter.Bridge.Protocol.Envelope
import           HsJupyter.Runtime.Manager (RuntimeManager)
import qualified HsJupyter.Runtime.Manager as RM
import           HsJupyter.Runtime.SessionState

-- | Main entry point for routing messages from the shell and control channels.
routeRequest :: (KatipContext m)
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

handleKernelInfo :: RuntimeManager -> ProtocolEnvelope Value -> IO (ProtocolEnvelope KernelInfoReply)
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
  return $ replyEnvelope envelope "kernel_info_reply" reply

handleExecuteRequest :: (KatipContext m)
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
            , erUserExpressions = Aeson.object []
            }
      return $ Right [replyEnvelope envelope "execute_reply" reply]

handleInterrupt :: RuntimeManager -> ProtocolEnvelope Value -> IO (ProtocolEnvelope InterruptReply)
handleInterrupt manager envelope = do
  RM.enqueueInterrupt manager (msgId (envelopeHeader envelope))
  return $ replyEnvelope envelope "interrupt_reply" (InterruptReply "ok")

handleShutdown :: RuntimeManager -> ProtocolEnvelope Value -> IO (ProtocolEnvelope ShutdownReply)
handleShutdown manager envelope =
  case Aeson.fromJSON (envelopeContent envelope) of
    Aeson.Error _ ->
      -- This shouldn't happen for a valid shutdown request
      return $ replyEnvelope envelope "shutdown_reply" (ShutdownReply False)
    Aeson.Success (req :: ShutdownRequest) -> do
      RM.enqueueShutdown manager (srRestart req)
      return $ replyEnvelope envelope "shutdown_reply" (ShutdownReply (srRestart req))

-- | Helper to create a reply envelope.
replyEnvelope :: (Aeson.ToJSON a)
              => ProtocolEnvelope any
              -> Text
              -> a
              -> ProtocolEnvelope a
replyEnvelope reqEnv newMsgType content =
  let reqHdr = envelopeHeader reqEnv
      newHdr = MessageHeader
        { identifiers = []
        , parentHeader = Just reqHdr
        , metadata = Aeson.object []
        , msgId = "reply-" <> msgId reqHdr -- TODO: Generate proper UUID
        , session = session reqHdr
        , username = username reqHdr
        , date = date reqHdr -- TODO: Update timestamp
        , msgType = newMsgType
        , version = version reqHdr
        }
  in ProtocolEnvelope
       { envelopeIdentities = envelopeIdentities reqEnv
       , envelopeHeader = newHdr
       , envelopeParent = Just reqHdr
       , envelopeMetadata = Aeson.object []
       , envelopeContent = content
       , envelopeSignature = "" -- The bridge will sign this
       , envelopeSignaturePayload = undefined -- The bridge will create this
       }
