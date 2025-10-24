{-# LANGUAGE OverloadedStrings #-}

module ExecuteEchoSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import HsJupyter.Bridge.Protocol.Codec (
  parseEnvelopeFrames,
  renderEnvelopeFrames,
 )
import HsJupyter.Bridge.Protocol.Envelope (
  MessageHeader (..),
  ProtocolEnvelope (..),
  emptyMetadata,
 )
import HsJupyter.KernelProcess (
  KernelProcessConfig (..),
  LogLevel (..),
  withKernel,
 )
import Network.Socket (
  Family (AF_INET),
  SockAddr (..),
  Socket,
  SocketType (Stream),
  bind,
  close,
  defaultProtocol,
  getSocketName,
  listen,
  socket,
  withSocketsDo,
 )
import qualified System.ZMQ4 as Z
import Test.Hspec

getFreePort :: IO Int
getFreePort = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 0 0) -- 0 => pede porta aleatória
  listen sock 1
  SockAddrInet port _ <- getSocketName sock
  close sock
  pure (fromIntegral port)

spec :: Spec
spec = describe "Execute echo integration" $ do
  it "returns execute_reply and stream envelopes via ZeroMQ" $ do
    now <- getCurrentTime
    shellPort <- getFreePort
    iopubPort <- getFreePort
    stdinPort <- getFreePort
    heartbeatPort <- getFreePort
    controlPort <- getFreePort

    let cfg =
          KernelProcessConfig
            { connectionFile = "./tmp/connection.json"
            , transport = T.pack "tcp"
            , ipAddress = T.pack "127.0.0.1"
            , key = BS8.pack "secret"
            , signatureScheme = T.pack "hmac-sha256"
            , shellPort = shellPort
            , iopubPort = iopubPort
            , stdinPort = stdinPort
            , heartbeatPort = heartbeatPort
            , controlPort = controlPort
            , logLevel = LogInfo
            , createdAt = now
            }

        shellEndpoint = "tcp://127.0.0.1:" <> show shellPort
        iopubEndpoint = "tcp://127.0.0.1:" <> show iopubPort

    withKernel cfg $ do
      Z.withContext $ \clientCtx ->
        Z.withSocket clientCtx Z.Dealer $ \shellClient ->
          Z.withSocket clientCtx Z.Sub $ \iopubClient -> do
            Z.setIdentity (Z.restrict (BS8.pack "client")) shellClient
            Z.connect shellClient shellEndpoint
            Z.connect iopubClient iopubEndpoint
            Z.subscribe iopubClient (BS8.pack "")
            threadDelay 200000 -- allow sockets to connect
            let env = buildExecuteRequest now
                frames = renderEnvelopeFrames (key cfg) env

            case NE.nonEmpty frames of
              Nothing -> expectationFailure "no frames generated for execute_request"
              Just ne -> Z.sendMulti shellClient ne
            replyFrames <- Z.receiveMulti shellClient
            replyEnvelope <- either (fail . show) pure (parseEnvelopeFrames replyFrames)
            msgType (envelopeHeader replyEnvelope) `shouldBe` T.pack "execute_reply"

            streamFrames <- Z.receiveMulti iopubClient
            streamEnvelope <- either (fail . show) pure (parseEnvelopeFrames streamFrames)
            msgType (envelopeHeader streamEnvelope) `shouldBe` T.pack "stream"

buildExecuteRequest :: UTCTime -> ProtocolEnvelope Aeson.Value
buildExecuteRequest now =
  let header =
        MessageHeader
          { msgId = T.pack "integration-msg"
          , session = T.pack "integration"
          , username = T.pack "tester"
          , msgType = T.pack "execute_request"
          , version = T.pack "5.3"
          , date = Just now
          }
      content =
        object
          [ "code" .= T.pack "print(\"ok\")"
          , "silent" .= False
          ]
   in ProtocolEnvelope
        { envelopeIdentities = []
        , envelopeHeader = header
        , envelopeParent = Nothing
        , envelopeMetadata = emptyMetadata
        , envelopeContent = content
        , envelopeSignature = T.empty
        }
