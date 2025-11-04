module HsJupyter.Bridge.Protocol.Codec
  ( computeSignature
  , verifySignature
  , encodeEnvelope
  , decodeExecuteRequest
  , EnvelopeFrameError(..)
  , parseEnvelopeFrames
  , renderEnvelopeFrames
  ) where


import           Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Control.Monad (when)

import           HsJupyter.Bridge.Protocol.Envelope
import           HsJupyter.Bridge.Protocol.Signature (computeSignature, verifySignature)



-- | Encode an envelope for logging or transmission (without ZeroMQ framing).
encodeEnvelope :: ProtocolEnvelope Value -> Value
encodeEnvelope env = Aeson.object
  [ "identities" .= (TE.decodeUtf8 . B64.encode <$> envelopeIdentities env)
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
  -- Find the delimiter
  let (ids, rest) = break (== TE.encodeUtf8 "<IDS|MSG>") frames
  
  -- Check for delimiter and correct number of payload frames
  case rest of
    (_delim:payload) -> do
      case payload of
        [sig, hdr, parent, meta, content] -> do
          parsePayload ids sig hdr parent meta content
        _ -> Left $ EnvelopeFrameError $ "Invalid payload frame count: " <> T.pack (show (length payload))
    _ -> Left $ EnvelopeFrameError "Missing <IDS|MSG> delimiter"

  where
    parsePayload identities sigBS headerBS parentBS metadataBS contentBS = do
      signature   <- decodeUtf8 sigBS
      header      <- decodeJSON "header" headerBS
      parentHeader <- decodeJSON "parent" parentBS
      metadata    <- decodeValue "metadata" metadataBS
      content     <- decodeValue "content" contentBS
      let payload = SignaturePayload
            { spHeader = headerBS
            , spParent = parentBS
            , spMetadata = metadataBS
            , spContent = contentBS
            }
      pure ProtocolEnvelope
        { envelopeIdentities = identities
        , envelopeHeader     = header
        , envelopeParent     = parentHeader
        , envelopeMetadata   = metadata
        , envelopeContent    = content
        , envelopeSignature  = signature
        , envelopeSignaturePayload = payload
        }
    decodeUtf8 :: BS.ByteString -> Either EnvelopeFrameError Text
    decodeUtf8 bs = first (EnvelopeFrameError . T.pack . show) (TE.decodeUtf8' bs)

    decodeJSON :: Aeson.FromJSON a => Text -> BS.ByteString -> Either EnvelopeFrameError a
    decodeJSON name bs = first (EnvelopeFrameError . (\e -> "Failed to decode " <> name <> ": " <> T.pack e)) (Aeson.eitherDecodeStrict bs)

    decodeValue :: Text -> BS.ByteString -> Either EnvelopeFrameError Value
    decodeValue = decodeJSON

-- | Render a protocol envelope into ZeroMQ multipart frames (identities + delimiter + HMAC + JSON payloads).
renderEnvelopeFrames :: BS.ByteString -> ProtocolEnvelope Value -> [BS.ByteString]
renderEnvelopeFrames key env =
  let payload = envelopeSignaturePayload env
      signatureText = computeSignature key payload
      signatureFrame = TE.encodeUtf8 signatureText
      headerFrame    = spHeader payload
      parentFrame    = spParent payload
      metadataFrame  = spMetadata payload
      contentFrame   = spContent payload
  in envelopeIdentities env
      ++ [TE.encodeUtf8 "<IDS|MSG>", signatureFrame, headerFrame, parentFrame, metadataFrame, contentFrame]
