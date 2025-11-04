{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Bridge.Protocol.Signature
  ( computeSignature
  , verifySignature
  ) where

import           Crypto.Hash (Digest, SHA256)
import           Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as T

import           HsJupyter.Bridge.Protocol.Envelope

-- | Compute a deterministic HMAC-SHA256 signature for the envelope frames.
computeSignature :: BS.ByteString -> SignaturePayload -> Text
computeSignature key payload
  | BS.null key = ""
  | otherwise   =
      let frames = payloadFrames payload
          digest :: Digest SHA256
          digest = hmacGetDigest (hmac key (BS.concat frames) :: HMAC SHA256)
      in T.pack (show digest)

-- | Verify the provided signature matches the recomputed value.
verifySignature :: BS.ByteString -> ProtocolEnvelope any -> Bool
verifySignature key env
  | BS.null key = True
  | otherwise   =
      let expected = computeSignature key (envelopeSignaturePayload env)
          actual   = envelopeSignature env
      in expected == actual
