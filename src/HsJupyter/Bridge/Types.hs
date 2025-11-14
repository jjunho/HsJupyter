module HsJupyter.Bridge.Types
  ( BridgeError(..)
  ) where

import           Data.Text (Text)

-- | Failures surfaced to callers for error handling/logging.
data BridgeError
  = SignatureValidationFailed
  | DecodeFailure Text
  deriving (Eq, Show)
