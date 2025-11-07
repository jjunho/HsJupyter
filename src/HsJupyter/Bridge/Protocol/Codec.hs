module HsJupyter.Bridge.Protocol.Codec
  ( canonicalJSON
  , computeSignature
  , verifySignature
  , encodeEnvelope
  , decodeExecuteRequest
  , EnvelopeFrameError(..)
  , parseEnvelopeFrames
  , renderEnvelopeFrames
  ) where

import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson (Value, encode, (.=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import HsJupyter.Bridge.Protocol.Envelope
  ( ExecuteRequest
  , ProtocolEnvelope(..)
  )

-- | Canonical JSON encoding (UTF-8, no whitespace) used for signing.
canonicalJSON :: Value -> BS.ByteString
canonicalJSON = LBS.toStrict . encode

-- | Compute a deterministic HMAC-SHA256 signature for the envelope frames.
computeSignature :: BS.ByteString -> ProtocolEnvelope Value -> Text
computeSignature key env
  | BS.null key = ""
  | otherwise   =
      let frames =
            [ canonicalJSON (Aeson.toJSON (envelopeHeader env))
            , canonicalJSON (maybe Aeson.Null Aeson.toJSON (envelopeParent env))
            , canonicalJSON (envelopeMetadata env)
            , canonicalJSON (envelopeContent env)
            ]
          digest :: Digest SHA256
          digest = hmacGetDigest (hmac key (BS.concat frames) :: HMAC SHA256)
      in T.pack (show digest)

-- | Verify the provided signature matches the recomputed value.
verifySignature :: BS.ByteString -> ProtocolEnvelope Value -> Bool
verifySignature key env
  | BS.null key = True
  | otherwise   =
      let expected = map toLower . T.unpack $ computeSignature key env
          actual   = map toLower . T.unpack $ envelopeSignature env
      in expected == actual

-- | Encode an envelope for logging or transmission (without ZeroMQ framing).
encodeEnvelope :: ProtocolEnvelope Value -> Value
encodeEnvelope env = Aeson.object
  [ "identities" .= envelopeIdentities env
  , "header" .= envelopeHeader env
  , "parent" .= envelopeParent env
  , "metadata" .= envelopeMetadata env
  , "content" .= envelopeContent env
  , "signature" .= envelopeSignature env
  ]

-- | Decode the content into an execute request when applicable.
decodeExecuteRequest :: Value -> Maybe ExecuteRequest
decodeExecuteRequest value =
  case Aeson.fromJSON value of
    Aeson.Success req -> Just req
    Aeson.Error _     -> Nothing

-- | Errors raised while decoding multipart ZeroMQ frames.
newtype EnvelopeFrameError = EnvelopeFrameError Text
  deriving (Eq, Show)

-- | Parse a multipart ZeroMQ message into a protocol envelope.
parseEnvelopeFrames :: [BS.ByteString] -> Either EnvelopeFrameError (ProtocolEnvelope Value)
parseEnvelopeFrames frames = do
  let (identityFrames, restWithDelim) = break BS.null frames
  rest <- case restWithDelim of
    []         -> Left $ EnvelopeFrameError "Missing frame delimiter"
    (_:remain) -> Right remain
  case rest of
    [sigBS, headerBS, parentBS, metadataBS, contentBS] -> do
      identities <- traverse decodeUtf8 identityFrames
      signature   <- decodeUtf8 sigBS
      header      <- decodeJSON headerBS
      parentValue <- decodeValue parentBS
      metadata    <- decodeValue metadataBS
      content     <- decodeValue contentBS
      parentHeader <- case parentValue of
        Aeson.Null -> Right Nothing
        other      -> case Aeson.fromJSON other of
          Aeson.Success hdr -> Right (Just hdr)
          Aeson.Error err   -> Left $ EnvelopeFrameError (T.pack err)
      pure ProtocolEnvelope
        { envelopeIdentities = identities
        , envelopeHeader     = header
        , envelopeParent     = parentHeader
        , envelopeMetadata   = metadata
        , envelopeContent    = content
        , envelopeSignature  = signature
        }
    _ -> Left $ EnvelopeFrameError "Unexpected frame count"
  where
    decodeUtf8 :: BS.ByteString -> Either EnvelopeFrameError Text
    decodeUtf8 bs = first (EnvelopeFrameError . T.pack . show) (TE.decodeUtf8' bs)

    decodeJSON :: Aeson.FromJSON a => BS.ByteString -> Either EnvelopeFrameError a
    decodeJSON bs = first (EnvelopeFrameError . T.pack) (Aeson.eitherDecodeStrict bs)

    decodeValue :: BS.ByteString -> Either EnvelopeFrameError Value
    decodeValue = decodeJSON

-- | Render a protocol envelope into ZeroMQ multipart frames (identities + delimiter + HMAC + JSON payloads).
renderEnvelopeFrames :: BS.ByteString -> ProtocolEnvelope Value -> [BS.ByteString]
renderEnvelopeFrames key env =
  let signatureText = computeSignature key env
      signatureFrame = TE.encodeUtf8 signatureText
      headerFrame    = canonicalJSON (Aeson.toJSON (envelopeHeader env))
      parentFrame    = canonicalJSON (maybe Aeson.Null Aeson.toJSON (envelopeParent env))
      metadataFrame  = canonicalJSON (envelopeMetadata env)
      contentFrame   = canonicalJSON (envelopeContent env)
      identityFrames = fmap TE.encodeUtf8 (envelopeIdentities env)
  in identityFrames
      ++ [BS.empty, signatureFrame, headerFrame, parentFrame, metadataFrame, contentFrame]
