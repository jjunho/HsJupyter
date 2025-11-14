{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Bridge.Protocol.Envelope
  ( ProtocolEnvelope(..)
  , MessageHeader(..)
  , SignaturePayload(..)
  , ExecuteRequest(..)
  , ExecuteReply(..)
  , ExecuteStatus(..)
  , KernelInfoRequest(..)
  , KernelInfoReply(..)
  , LanguageInfo(..)
  , InterruptReply(..)
  , ShutdownRequest(..)
  , ShutdownReply(..)
  , payloadFrames
  , signaturePayloadFrom
  , emptyMetadata
  , fromExecuteRequest
  , fromKernelInfoRequest
  , toExecuteReply
  , fromKernelInfoRequest
  , toKernelInfoReply
  ) where

import           Data.Aeson (Value, (.:), (.:?), (.=), (.!=), object, withObject, withText)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)

-- | Protocol envelope capturing all message frames
data ProtocolEnvelope content = ProtocolEnvelope
  { envelopeIdentities       :: [BS.ByteString]
  , envelopeHeader           :: MessageHeader
  , envelopeParent           :: Maybe MessageHeader
  , envelopeMetadata         :: Value
  , envelopeContent          :: content
  , envelopeSignature        :: Text
  , envelopeSignaturePayload :: SignaturePayload
  } deriving (Eq, Show)

-- | Message header per Jupyter protocol
data MessageHeader = MessageHeader
  { msgId    :: Text
  , msgType  :: Text
  , session  :: Text
  , username :: Text
  , version  :: Text
  , date     :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON MessageHeader where
  parseJSON = withObject "MessageHeader" $ \obj ->
    MessageHeader
      <$> obj .:  "msg_id"
      <*> obj .:  "msg_type"
      <*> obj .:  "session"
      <*> obj .:  "username"
      <*> obj .:  "version"
      <*> obj .:? "date"

instance Aeson.ToJSON MessageHeader where
  toJSON header = object
    [ "msg_id" .= msgId header
    , "msg_type" .= msgType header
    , "session" .= session header
    , "username" .= username header
    , "version" .= version header
    , "date" .= date header
    ]

-- | Signature payload for HMAC computation
data SignaturePayload = SignaturePayload
  { spHeader   :: BS.ByteString
  , spParent   :: BS.ByteString
  , spMetadata :: BS.ByteString
  , spContent  :: BS.ByteString
  } deriving (Eq, Show)

payloadFrames :: SignaturePayload -> [BS.ByteString]
payloadFrames (SignaturePayload header parent meta content) =
  [header, parent, meta, content]

signaturePayloadFrom
  :: MessageHeader
  -> Maybe MessageHeader
  -> Value
  -> Value
  -> SignaturePayload
signaturePayloadFrom header parent metadata content = SignaturePayload
  { spHeader = encodeStrict header
  , spParent = encodeStrict (maybe Aeson.Null Aeson.toJSON parent)
  , spMetadata = encodeStrict metadata
  , spContent = encodeStrict content
  }

encodeStrict :: Aeson.ToJSON a => a -> BS.ByteString
encodeStrict = LBS.toStrict . Aeson.encode

emptyMetadata :: Value
emptyMetadata = Aeson.Object mempty

-- | Execute request from frontend
data ExecuteRequest = ExecuteRequest
  { erCode            :: Text
  , erSilent          :: Bool
  , erAllowStdin      :: Bool
  , erStoreHistory    :: Bool
  , erUserExpressions :: Value
  } deriving (Eq, Show)

instance Aeson.FromJSON ExecuteRequest where
  parseJSON = withObject "ExecuteRequest" $ \obj ->
    ExecuteRequest
      <$> obj .: "code"
      <*> obj .:? "silent" .!= False
      <*> obj .:? "allow_stdin" .!= False
      <*> obj .:? "store_history" .!= True
      <*> obj .:? "user_expressions" .!= object []

instance Aeson.ToJSON ExecuteRequest where
  toJSON req = object
    [ "code" .= erCode req
    , "silent" .= erSilent req
    , "allow_stdin" .= erAllowStdin req
    , "store_history" .= erStoreHistory req
    , "user_expressions" .= erUserExpressions req
    ]

-- | Execution status
data ExecuteStatus = ExecuteOk | ExecuteError deriving (Eq, Show)

executeStatusToText :: ExecuteStatus -> Text
executeStatusToText ExecuteOk    = "ok"
executeStatusToText ExecuteError = "error"

textToExecuteStatus :: Text -> ExecuteStatus
textToExecuteStatus t = if T.toLower t == "ok" then ExecuteOk else ExecuteError

instance Aeson.ToJSON ExecuteStatus where
  toJSON = Aeson.String . executeStatusToText

instance Aeson.FromJSON ExecuteStatus where
  parseJSON = withText "ExecuteStatus" (pure . textToExecuteStatus)

-- | Execute reply to frontend
data ExecuteReply = ExecuteReply
  { erStatus               :: Text
  , erExecutionCount       :: Int
  , erPayload              :: [Value]
  , erReplyUserExpressions :: Value
  } deriving (Eq, Show)

instance Aeson.ToJSON ExecuteReply where
  toJSON reply = object
    [ "status" .= erStatus reply
    , "execution_count" .= erExecutionCount reply
    , "payload" .= erPayload reply
    , "user_expressions" .= erReplyUserExpressions reply
    ]

instance Aeson.FromJSON ExecuteReply where
  parseJSON = withObject "ExecuteReply" $ \obj ->
    ExecuteReply
      <$> obj .: "status"
      <*> obj .: "execution_count"
      <*> obj .:? "payload" .!= []
      <*> obj .:? "user_expressions" .!= object []

-- | Kernel info request (empty payload)
data KernelInfoRequest = KernelInfoRequest
  deriving (Eq, Show)

instance Aeson.FromJSON KernelInfoRequest where
  parseJSON = withObject "KernelInfoRequest" $ \_ ->
    pure KernelInfoRequest

instance Aeson.ToJSON KernelInfoRequest where
  toJSON KernelInfoRequest = object []

-- | Language info for kernel_info_reply
data LanguageInfo = LanguageInfo
  { liName          :: Text
  , liVersion       :: Text
  , liMimetype      :: Text
  , liFileExtension :: Text
  } deriving (Eq, Show)

instance Aeson.ToJSON LanguageInfo where
  toJSON lang = object
    [ "name" .= liName lang
    , "version" .= liVersion lang
    , "mimetype" .= liMimetype lang
    , "file_extension" .= liFileExtension lang
    ]

instance Aeson.FromJSON LanguageInfo where
  parseJSON = withObject "LanguageInfo" $ \obj ->
    LanguageInfo
      <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .: "mimetype"
      <*> obj .: "file_extension"

-- | Kernel info reply with metadata
data KernelInfoReply = KernelInfoReply
  { protocolVersion       :: Text
  , implementation        :: Text
  , implementationVersion :: Text
  , languageInfo          :: LanguageInfo
  , banner                :: Text
  } deriving (Eq, Show)

instance Aeson.ToJSON KernelInfoReply where
  toJSON reply = object
    [ "protocol_version" .= protocolVersion reply
    , "implementation" .= implementation reply
    , "implementation_version" .= implementationVersion reply
    , "language_info" .= languageInfo reply
    , "banner" .= banner reply
    , "status" .= ("ok" :: Text)
    ]

instance Aeson.FromJSON KernelInfoReply where
  parseJSON = withObject "KernelInfoReply" $ \obj ->
    KernelInfoReply
      <$> obj .: "protocol_version"
      <*> obj .: "implementation"
      <*> obj .: "implementation_version"
      <*> obj .: "language_info"
      <*> obj .: "banner"

-- | Interrupt reply
data InterruptReply = InterruptReply { interruptStatus :: Text }
  deriving (Eq, Show)

instance Aeson.ToJSON InterruptReply where
  toJSON (InterruptReply st) = object ["status" .= st]

instance Aeson.FromJSON InterruptReply where
  parseJSON = withObject "InterruptReply" $ \obj ->
    InterruptReply <$> obj .:? "status" .!= "ok"

-- | Shutdown request
newtype ShutdownRequest = ShutdownRequest
  { srRestart :: Bool
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON ShutdownRequest where
  parseJSON = withObject "ShutdownRequest" $ \obj ->
    ShutdownRequest <$> obj .:? "restart" .!= False

instance Aeson.ToJSON ShutdownRequest where
  toJSON (ShutdownRequest restart) = object ["restart" .= restart]

-- | Shutdown reply
newtype ShutdownReply = ShutdownReply
  { srRestart :: Bool
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON ShutdownReply where
  toJSON (ShutdownReply restart) = object ["restart" .= restart]

instance Aeson.FromJSON ShutdownReply where
  parseJSON = withObject "ShutdownReply" $ \obj ->
    ShutdownReply <$> obj .:? "restart" .!= False

-- Helper functions

fromExecuteRequest :: ProtocolEnvelope Value -> Maybe (ProtocolEnvelope ExecuteRequest)
fromExecuteRequest env =
  if msgType (envelopeHeader env) == "execute_request"
    then case Aeson.fromJSON (envelopeContent env) of
      Aeson.Success payload -> Just env { envelopeContent = payload }
      Aeson.Error _         -> Nothing
    else Nothing

toExecuteReply :: ProtocolEnvelope ExecuteRequest -> ExecuteReply -> ProtocolEnvelope Value
toExecuteReply env reply = env
  { envelopeContent = Aeson.toJSON reply
  , envelopeHeader = (envelopeHeader env) { msgType = "execute_reply" }
  , envelopeSignaturePayload = signaturePayloadFrom
      ((envelopeHeader env) { msgType = "execute_reply" })
      (envelopeParent env)
      (envelopeMetadata env)
      (Aeson.toJSON reply)
  }

fromKernelInfoRequest :: ProtocolEnvelope Value -> Maybe (ProtocolEnvelope KernelInfoRequest)
fromKernelInfoRequest env =
  if msgType (envelopeHeader env) == "kernel_info_request"
    then Just env { envelopeContent = KernelInfoRequest }
    else Nothing

toKernelInfoReply :: ProtocolEnvelope KernelInfoRequest -> KernelInfoReply -> ProtocolEnvelope Value
toKernelInfoReply env reply = env
  { envelopeContent = Aeson.toJSON reply
  , envelopeParent = Just (envelopeHeader env)
  , envelopeHeader = (envelopeHeader env) { msgType = "kernel_info_reply" }
  , envelopeSignaturePayload = signaturePayloadFrom
      ((envelopeHeader env) { msgType = "kernel_info_reply" })
      (Just (envelopeHeader env))
      (envelopeMetadata env)
      (Aeson.toJSON reply)
  }
