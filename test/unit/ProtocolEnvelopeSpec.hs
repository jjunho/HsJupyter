{-# LANGUAGE OverloadedStrings #-}

module ProtocolEnvelopeSpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Test.Hspec

import HsJupyter.Bridge.Protocol.Envelope
  ( ExecuteReply(..)
  , ExecuteRequest(..)
  , ExecuteStatus(..)
  , MessageHeader(..)
  , ProtocolEnvelope(..)
  , emptyMetadata
  , fromExecuteRequest
  , signaturePayloadFrom
  , toExecuteReply
  )

spec :: Spec
spec = do
  describe "ProtocolEnvelope" $ do
    it "decodes execute_request payloads" $ do
      env <- sampleEnvelope
      let typed = fromExecuteRequest env
      fmap (erCode . envelopeContent) typed `shouldBe` Just "print(\"ok\")"

    it "converts to execute_reply preserving identities" $ do
      env <- sampleEnvelope
      case fromExecuteRequest env of
        Nothing -> expectationFailure "could not decode execute_request"
        Just typed -> do
          let reply = ExecuteReply 1 ExecuteOk []
              out = toExecuteReply typed reply
          envelopeIdentities out `shouldBe` envelopeIdentities env
          msgType (envelopeHeader out) `shouldBe` T.pack "execute_reply"

sampleEnvelope :: IO (ProtocolEnvelope Aeson.Value)
sampleEnvelope = do
  now <- getCurrentTime
  let header = MessageHeader
        { msgId = T.pack "abc"
        , session = T.pack "sess"
        , username = T.pack "user"
        , msgType = T.pack "execute_request"
        , version = T.pack "5.3"
        , date = Just now
        }
      payload = object
        [ "code" .= T.pack "print(\"ok\")"
        ]
  pure ProtocolEnvelope
    { envelopeIdentities = [BS.pack "id"]
    , envelopeHeader = header
    , envelopeParent = Nothing
    , envelopeMetadata = emptyMetadata
    , envelopeContent = payload
    , envelopeSignature = T.empty
    , envelopeSignaturePayload = signaturePayloadFrom header Nothing emptyMetadata payload
    }
