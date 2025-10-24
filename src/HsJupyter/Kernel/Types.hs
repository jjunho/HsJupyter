module HsJupyter.Kernel.Types
  ( LogLevel(..)
  , KernelProcessConfig(..)
  , KernelProcessState(..)
  , LoadConfigError(..)
  , summariseConfig
  , shouldLog
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.Maybe (fromMaybe)

-- | Structured log level independent of any particular logging framework.
data LogLevel
  = LogDebug
  | LogInfo
  | LogWarn
  | LogError
  deriving (Eq, Ord, Show, Read)

-- | Configuration parsed from a Jupyter connection file augmented with CLI flags.
data KernelProcessConfig = KernelProcessConfig
  { connectionFile  :: FilePath
  , transport       :: Text
  , ipAddress       :: Text
  , key             :: BS.ByteString
  , signatureScheme :: Text
  , shellPort       :: Int
  , iopubPort       :: Int
  , stdinPort       :: Int
  , heartbeatPort   :: Int
  , controlPort     :: Int
  , logLevel        :: LogLevel
  , createdAt       :: UTCTime
  } deriving (Show)

-- | Lifecycle snapshots useful for observability.
data KernelProcessState
  = Uninitialised
  | BindingSockets
  | Running
  | Draining
  | Terminated
  deriving (Eq, Show)

-- | Config parsing failure with context to aid debugging.
data LoadConfigError
  = ConfigFileMissing FilePath
  | ConfigDecodeError String
  | ConfigValidationError String
  deriving (Eq, Show)

-- | Produce a single-line summary for logging.
summariseConfig :: KernelProcessConfig -> Text
summariseConfig cfg =
  T.intercalate " "
    [ "connection=" <> T.pack (connectionFile cfg)
    , "transport=" <> transport cfg
    , "ip=" <> ipAddress cfg
    , "shell=" <> port shellPort
    , "iopub=" <> port iopubPort
    , "control=" <> port controlPort
    ]
  where
    port f = T.pack . show $ f cfg

-- | Determine whether the desired log level should be emitted when comparing to current filter.
shouldLog :: LogLevel -> LogLevel -> Bool
shouldLog current threshold = current >= threshold

instance FromJSON KernelProcessConfig where
  parseJSON = withObject "KernelProcessConfig" $ \obj -> do
    transport' <- obj .: "transport"
    ip'        <- obj .: "ip"
    key'       <- obj .:? "key"
    sigScheme  <- obj .:? "signature_scheme"
    shellP     <- obj .: "shell_port"
    iopubP     <- obj .: "iopub_port"
    stdinP     <- obj .: "stdin_port"
    hbP        <- obj .: "hb_port"
    ctrlP      <- obj .: "control_port"
    logLevelRaw <- obj .:? "log_level"
    pure KernelProcessConfig
      { connectionFile  = ""
      , transport       = transport'
      , ipAddress       = ip'
      , key             = maybe BS.empty BS.pack key'
      , signatureScheme = fromMaybe "hmac-sha256" sigScheme
      , shellPort       = shellP
      , iopubPort       = iopubP
      , stdinPort       = stdinP
      , heartbeatPort   = hbP
      , controlPort     = ctrlP
      , logLevel        = parseLogLevel logLevelRaw
      , createdAt       = epoch
      }
epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

parseLogLevel :: Maybe Text -> LogLevel
parseLogLevel Nothing = LogInfo
parseLogLevel (Just txt) = case T.toLower txt of
  "debug" -> LogDebug
  "warn"  -> LogWarn
  "error" -> LogError
  "info"  -> LogInfo
  _        -> LogInfo

