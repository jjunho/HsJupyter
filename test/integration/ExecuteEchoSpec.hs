{-# LANGUAGE OverloadedStrings #-}

module ExecuteEchoSpec (spec) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, cancel)
import           Control.Monad (replicateM_, void)
import           Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Katip
import           System.IO (stderr)
import qualified System.ZMQ4 as Z
import           Test.Hspec

import           HsJupyter.Bridge.Protocol.Codec
import           HsJupyter.Bridge.Protocol.Envelope
import           HsJupyter.KernelProcess

spec :: Spec
spec = around withKernelFixture $ do
  describe "Execute echo integration" $ do
    it "returns execute_reply and stream envelopes via ZeroMQ" $ \(cfg, shell, iopub) -> do
      let env = buildExecuteRequest (createdAt cfg) "print(\"ok\")"
          frames = renderEnvelopeFrames (key cfg) env
      
      liftIO $ Z.sendMulti shell (NE.fromList frames)
      
      replyFrames <- liftIO $ Z.receiveMulti shell
      replyEnvelope <- either (fail . show) pure (parseEnvelopeFrames replyFrames)
      msgType (envelopeHeader replyEnvelope) `shouldBe` "execute_reply"
      
      streamEnvelope <- awaitEnvelope iopub 5 "stream"
      msgType (envelopeHeader streamEnvelope) `shouldBe` "stream"
      let String streamContent = (envelopeContent streamEnvelope Aeson..= "text")
      streamContent `shouldBe` "ok\n"

    it "handles interrupt requests" $ \(cfg, shell, iopub) -> do
      let env = buildExecuteRequest (createdAt cfg) "import Control.Concurrent\nforever (threadDelay 100000)"
          frames = renderEnvelopeFrames (key cfg) env
      
      -- This will block, so run it in a separate thread
      execAsync <- async $ Z.sendMulti shell (NE.fromList frames)
      
      -- Give it a moment to start
      threadDelay 200000
      
      -- Send interrupt
      let intEnv = buildInterruptRequest (createdAt cfg)
          intFrames = renderEnvelopeFrames (key cfg) intEnv
      Z.sendMulti shell (NE.fromList intFrames)
      
      -- Check for interrupt reply
      intReplyFrames <- Z.receiveMulti shell
      intReplyEnvelope <- either (fail . show) pure (parseEnvelopeFrames intReplyFrames)
      msgType (envelopeHeader intReplyEnvelope) `shouldBe` "interrupt_reply"
      
      -- The original execution should have been cancelled
      -- We should get a reply with status "aborted"
      replyFrames <- Z.receiveMulti shell
      replyEnvelope <- either (fail . show) pure (parseEnvelopeFrames replyFrames)
      let String status = (envelopeContent replyEnvelope Aeson..= "status")
      status `shouldBe` "aborted"
      
      cancel execAsync

withKernelFixture :: ((KernelProcessConfig, Z.Socket Z.Dealer, Z.Socket Z.Sub) -> IO ()) -> IO ()
withKernelFixture action = withSocketsDo $ do
  now <- getCurrentTime
  shellPort <- getFreePort
  iopubPort <- getFreePort
  stdinPort <- getFreePort
  heartbeatPort <- getFreePort
  controlPort <- getFreePort
  
  let cfg = KernelProcessConfig
        { connectionFile = "./tmp/connection.json"
        , transport = "tcp"
        , ipAddress = "127.0.0.1"
        , key = "secret"
        , signatureScheme = "hmac-sha256"
        , shellPort = shellPort
        , iopubPort = iopubPort
        , stdinPort = stdinPort
        , heartbeatPort = heartbeatPort
        , controlPort = controlPort
        , logLevel = LogDebug
        , createdAt = now
        }
      shellEndpoint = "tcp://127.0.0.1:" <> show shellPort
      iopubEndpoint = "tcp://127.0.0.1:" <> show iopubPort
  
  kernelAsync <- async $ withKernel cfg (forever (threadDelay 1000000))
  
  Z.withContext $ \clientCtx ->
    Z.withSocket clientCtx Z.Dealer $ \shellClient ->
      Z.withSocket clientCtx Z.Sub $ \iopubClient -> do
        Z.setIdentity (Z.restrict (BS8.pack "client")) shellClient
        Z.connect shellClient shellEndpoint
        Z.connect iopubClient iopubEndpoint
        Z.subscribe iopubClient ""
        threadDelay 200000 -- allow sockets to connect
        
        action (cfg, shellClient, iopubClient)
        
  cancel kernelAsync

awaitEnvelope :: Z.Socket Z.Sub -> Int -> Text -> IO (ProtocolEnvelope Value)
awaitEnvelope sock attempts expectedType
  | attempts <= 0 = fail $ "Did not observe envelope of type " <> T.unpack expectedType
  | otherwise = do
      frames <- Z.receiveMulti sock
      case parseEnvelopeFrames frames of
        Left err -> fail (show err)
        Right env ->
          if msgType (envelopeHeader env) == expectedType
            then pure env
            else awaitEnvelope sock (attempts - 1) expectedType

buildExecuteRequest :: UTCTime -> Text -> ProtocolEnvelope Value
buildExecuteRequest now code =
  let header = MessageHeader
        { msgId = "integration-msg"
        , session = "integration"
        , username = "tester"
        , msgType = "execute_request"
        , version = "5.3"
        , date = Just now
        , identifiers = []
        , parentHeader = Nothing
        , metadata = emptyMetadata
        }
      content = object [ "code" .= code, "silent" .= False ]
  in ProtocolEnvelope
       { envelopeIdentities = []
       , envelopeHeader = header
       , envelopeParent = Nothing
       , envelopeMetadata = emptyMetadata
       , envelopeContent = content
       , envelopeSignature = ""
       , envelopeSignaturePayload = signaturePayloadFrom header Nothing emptyMetadata content
       }

buildInterruptRequest :: UTCTime -> ProtocolEnvelope Value
buildInterruptRequest now =
  let header = MessageHeader
        { msgId = "integration-interrupt"
        , session = "integration"
        , username = "tester"
        , msgType = "interrupt_request"
        , version = "5.3"
        , date = Just now
        , identifiers = []
        , parentHeader = Nothing
        , metadata = emptyMetadata
        }
  in ProtocolEnvelope
       { envelopeIdentities = []
       , envelopeHeader = header
       , envelopeParent = Nothing
       , envelopeMetadata = emptyMetadata
       , envelopeContent = object []
       , envelopeSignature = ""
       , envelopeSignaturePayload = signaturePayloadFrom header Nothing emptyMetadata (object [])
       }

getFreePort :: IO Int
getFreePort = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 0 0)
  listen sock 1
  port <- socketPort sock
  close sock
  return $ fromIntegral port
