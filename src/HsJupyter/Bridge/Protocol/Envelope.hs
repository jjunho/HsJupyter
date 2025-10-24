module HsJupyter.Bridge.Protocol.Envelope
  ( Channel(..)
  , MessageHeader(..)
  , ProtocolEnvelope(..)
  , ExecuteRequest(..)
  , ExecuteReply(..)
  , InterruptReply(..)
  , ExecuteStatus(..)
  , fromExecuteRequest
  , toExecuteReply
  , emptyMetadata
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  , withObject
  , withText
  , object
  )
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

-- | Channels defined by the Jupyter messaging protocol.
data Channel = Shell | IOPub | Control | Stdin | Heartbeat
  deriving (Eq, Show)

-- | Header metadata per Jupyter message.
data MessageHeader = MessageHeader
  { msgId    :: Text
  , session  :: Text
  , username :: Text
  , msgType  :: Text
  , version  :: Text
  , date     :: Maybe UTCTime
  } deriving (Eq, Show)

instance FromJSON MessageHeader where
  parseJSON = withObject "MessageHeader" $ \obj ->
    MessageHeader
      <$> obj .: "msg_id"
      <*> obj .: "session"
      <*> obj .: "username"
      <*> obj .: "msg_type"
      <*> obj .: "version"
      <*> obj .:? "date"

instance ToJSON MessageHeader where
  toJSON hdr = object
    [ "msg_id" .= msgId hdr
    , "session" .= session hdr
    , "username" .= username hdr
    , "msg_type" .= msgType hdr
    , "version" .= version hdr
    , "date" .= date hdr
    ]

-- | Envelope capturing all message frames.
data ProtocolEnvelope content = ProtocolEnvelope
  { envelopeIdentities :: [Text]
  , envelopeHeader     :: MessageHeader
  , envelopeParent     :: Maybe MessageHeader
  , envelopeMetadata   :: Value
  , envelopeContent    :: content
  , envelopeSignature  :: Text
  } deriving (Eq, Show)

-- | Execute request payload subset used for the echo runtime.
data ExecuteRequest = ExecuteRequest
  { erCode         :: Text
  , erSilent       :: Bool
  , erAllowStdin   :: Bool
  , erStoreHistory :: Bool
  } deriving (Eq, Show)

instance FromJSON ExecuteRequest where
  parseJSON = withObject "ExecuteRequest" $ \obj ->
    ExecuteRequest
      <$> obj .: "code"
      <*> obj .:? "silent" .!= False
      <*> obj .:? "allow_stdin" .!= False
      <*> obj .:? "store_history" .!= True

instance ToJSON ExecuteRequest where
  toJSON req = object
    [ "code" .= erCode req
    , "silent" .= erSilent req
    , "allow_stdin" .= erAllowStdin req
    , "store_history" .= erStoreHistory req
    ]

-- | Execute reply payload produced by the runtime.
data ExecuteStatus = ExecuteOk | ExecuteError deriving (Eq, Show)

executeStatusToText :: ExecuteStatus -> Text
executeStatusToText ExecuteOk    = "ok"
executeStatusToText ExecuteError = "error"

textToExecuteStatus :: Text -> ExecuteStatus
textToExecuteStatus t = if T.toLower t == "ok" then ExecuteOk else ExecuteError

instance ToJSON ExecuteStatus where
  toJSON = Aeson.String . executeStatusToText

instance FromJSON ExecuteStatus where
  parseJSON = withText "ExecuteStatus" (pure . textToExecuteStatus)

-- | Reply payload body.
data ExecuteReply = ExecuteReply
  { executeReplyCount   :: Int
  , executeReplyStatus  :: ExecuteStatus
  , executeReplyPayload :: [Value]
  } deriving (Eq, Show)

instance ToJSON ExecuteReply where
  toJSON reply = object
    [ "status" .= executeReplyStatus reply
    , "execution_count" .= executeReplyCount reply
    , "payload" .= executeReplyPayload reply
    , "user_expressions" .= Aeson.Object mempty
    ]

instance FromJSON ExecuteReply where
  parseJSON = withObject "ExecuteReply" $ \obj ->
    ExecuteReply
      <$> obj .: "execution_count"
      <*> (textToExecuteStatus <$> obj .: "status")
      <*> obj .:? "payload" .!= []

-- | Interrupt reply on control channel.
data InterruptReply = InterruptReply { interruptStatus :: Text }
  deriving (Eq, Show)

instance ToJSON InterruptReply where
  toJSON (InterruptReply st) = object ["status" .= st]

instance FromJSON InterruptReply where
  parseJSON = withObject "InterruptReply" $ \obj ->
    InterruptReply <$> obj .:? "status" .!= "ok"

-- | Helper to convert a general envelope to an execute request when msg_type matches.
fromExecuteRequest :: ProtocolEnvelope Value -> Maybe (ProtocolEnvelope ExecuteRequest)
fromExecuteRequest env =
  if msgType (envelopeHeader env) == "execute_request"
    then case Aeson.fromJSON (envelopeContent env) of
      Aeson.Success payload -> Just env { envelopeContent = payload }
      Aeson.Error _         -> Nothing
    else Nothing

-- | Build an execute reply envelope from an execute request.
toExecuteReply :: ProtocolEnvelope ExecuteRequest -> ExecuteReply -> ProtocolEnvelope Value
toExecuteReply env reply = env
  { envelopeContent = toJSON reply
  , envelopeHeader = (envelopeHeader env)
      { msgType = "execute_reply" }
  }

-- | Utility for consumers needing empty metadata without re-importing aeson internals.
emptyMetadata :: Value
emptyMetadata = Aeson.Object mempty
