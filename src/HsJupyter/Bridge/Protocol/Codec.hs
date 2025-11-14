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
import qualified Data.Aeson.KeyMap as KM
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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
-- Accepts both Jupyter standard delimiter (empty frame) and legacy <IDS|MSG> string.
-- 
-- Jupyter protocol uses an empty frame as delimiter between identity frames
-- and message payload. This was fixed to accept both formats for compatibility.
-- Fix for: Kernel was rejecting all messages due to expecting wrong delimiter format.
parseEnvelopeFrames :: [BS.ByteString] -> Either EnvelopeFrameError (ProtocolEnvelope Value)
parseEnvelopeFrames frames = do
  -- Find the delimiter (empty frame or <IDS|MSG>)
  let isDelimiter frame = BS.null frame || frame == TE.encodeUtf8 "<IDS|MSG>"
      (ids, rest) = break isDelimiter frames
  
  -- Check for delimiter and correct number of payload frames
  case rest of
    (_delim:payload) -> do
      case payload of
        [sig, hdr, parent, meta, content] -> do
          parsePayload ids sig hdr parent meta content
        _ -> Left $ EnvelopeFrameError $ "Invalid payload frame count: " <> T.pack (show (length payload))
    _ -> Left $ EnvelopeFrameError "Missing delimiter (empty frame or <IDS|MSG>)"

  where
    parsePayload identities sigBS headerBS parentBS metadataBS contentBS = do
      signature   <- decodeUtf8 sigBS
      header      <- decodeJSON "header" headerBS
      parentHeader <- decodeParentHeader parentBS
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
    
    -- | Decode parent header, accepting empty object as Nothing.
    -- Jupyter protocol allows empty parent_header ({}) for initial messages
    -- that are not replies to previous messages.
    -- Fix for: Kernel was failing to parse messages with empty parent_header.
    decodeParentHeader :: BS.ByteString -> Either EnvelopeFrameError (Maybe MessageHeader)
    decodeParentHeader bs = do
      value <- decodeValue "parent_value" bs
      case value of
        Aeson.Object obj | KM.null obj -> Right Nothing  -- Empty object = no parent
        _ -> case Aeson.fromJSON value of
               Aeson.Success hdr -> Right (Just hdr)
               Aeson.Error err -> Left $ EnvelopeFrameError $ "Failed to decode parent: " <> T.pack err

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
      ++ [BS.empty, signatureFrame, headerFrame, parentFrame, metadataFrame, contentFrame]  -- Use empty frame as delimiter
