{-# LANGUAGE OverloadedStrings #-}

module ExecuteEchoSpec (spec) where

import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), object)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified System.ZMQ4 as Z
import Test.Hspec

import HsJupyter.Bridge.Protocol.Codec
  ( parseEnvelopeFrames
  , renderEnvelopeFrames
  )
import HsJupyter.Bridge.Protocol.Envelope
  ( MessageHeader(..)
  , ProtocolEnvelope(..)
  , emptyMetadata
  )
import HsJupyter.KernelProcess
  ( KernelProcessConfig(..)
  , LogLevel(..)
  , withKernel
  )

spec :: Spec
spec = describe "Execute echo integration" $ do
  it "returns execute_reply and stream envelopes via ZeroMQ" $ do
    now <- getCurrentTime
    let cfg = KernelProcessConfig
          { connectionFile = "./tmp/connection.json"
          , transport = T.pack "tcp"
          , ipAddress = T.pack "127.0.0.1"
          , key = BS8.pack "secret"
          , signatureScheme = T.pack "hmac-sha256"
          , shellPort = 5780
          , iopubPort = 5781
          , stdinPort = 5782
          , heartbeatPort = 5783
          , controlPort = 5784
          , logLevel = LogInfo
          , createdAt = now
          }
        shellEndpoint = "tcp://127.0.0.1:5780"
        iopubEndpoint = "tcp://127.0.0.1:5781"
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
  let header = MessageHeader
        { msgId = T.pack "integration-msg"
        , session = T.pack "integration"
        , username = T.pack "tester"
        , msgType = T.pack "execute_request"
        , version = T.pack "5.3"
        , date = Just now
        }
      content = object
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
