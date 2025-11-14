{-# LANGUAGE OverloadedStrings #-}

module SignatureValidationSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Test.Hspec

import HsJupyter.Bridge.Protocol.Codec (computeSignature, verifySignature)
import HsJupyter.Bridge.Protocol.Envelope
  ( MessageHeader(..)
  , ProtocolEnvelope(..)
  , emptyMetadata
  , signaturePayloadFrom
  )

spec :: Spec
spec = describe "Signature validation" $ do
  it "accepts envelopes with matching signature" $ do
    key <- pure (BS.pack "secret")
    env <- makeEnvelope
    let signature = computeSignature key (envelopeSignaturePayload env)
        signed = env { envelopeSignature = signature }
    verifySignature key signed `shouldBe` True

  it "rejects envelopes with invalid signature" $ do
    key <- pure (BS.pack "secret")
    env <- makeEnvelope
    let tampered = env { envelopeSignature = T.pack "mismatch" }
    verifySignature key tampered `shouldBe` False

makeEnvelope :: IO (ProtocolEnvelope Aeson.Value)
makeEnvelope = do
  now <- getCurrentTime
  let header = MessageHeader
        { msgId = T.pack "msg"
        , session = T.pack "sess"
        , username = T.pack "user"
        , msgType = T.pack "execute_request"
        , version = T.pack "5.3"
        , date = Just now
        }
      payload = Aeson.object ["code" Aeson..= T.pack "print(1)"]
      sigPayload = signaturePayloadFrom header Nothing emptyMetadata payload
  pure ProtocolEnvelope
    { envelopeIdentities = [BS.pack "id"]
    , envelopeHeader = header
    , envelopeParent = Nothing
    , envelopeMetadata = emptyMetadata
    , envelopeContent = payload
    , envelopeSignature = T.empty
    , envelopeSignaturePayload = sigPayload
    }
