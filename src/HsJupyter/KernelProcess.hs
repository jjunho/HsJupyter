module HsJupyter.KernelProcess
  ( module HsJupyter.Kernel.Types
  , loadKernelProcessConfig
  , withKernel
  , runKernel
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, cancel, link)
import           Control.Exception (bracket, displayException, try)
import           Control.Monad (forever, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (eitherDecodeFileStrict')
import           Data.Int (Int64)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory)
import           System.Exit (die)
import           System.FilePath ((</>))
import qualified System.ZMQ4 as Z
import           Katip

import           HsJupyter.Bridge.JupyterBridge
import           HsJupyter.Bridge.Protocol.Codec (EnvelopeFrameError(..), parseEnvelopeFrames, renderEnvelopeFrames)
import           HsJupyter.Bridge.Protocol.Signature (verifySignature)
import           HsJupyter.Bridge.Protocol.Envelope
import           HsJupyter.Bridge.HeartbeatThread (HeartbeatStatus(..))
import           HsJupyter.Kernel.Types
import           HsJupyter.Runtime.Manager (withRuntimeManager)

-- | Load and validate the kernel configuration from a connection file.
loadKernelProcessConfig :: FilePath -> Maybe LogLevel -> IO (Either LoadConfigError KernelProcessConfig)
loadKernelProcessConfig path overrideLevel = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left (ConfigFileMissing path)
    else do
      now <- getCurrentTime
      eDecoded <- eitherDecodeFileStrict' path
      case eDecoded of
        Left err -> pure $ Left (ConfigDecodeError err)
        Right cfg -> do
          let finalCfg = cfg
                { connectionFile = path
                , logLevel = fromMaybe (logLevel cfg) overrideLevel
                , createdAt = now
                }
          pure $ validateConfig finalCfg
  where
    validateConfig :: KernelProcessConfig -> Either LoadConfigError KernelProcessConfig
    validateConfig cfg
      | any (<= 0)
          [ shellPort cfg
          , iopubPort cfg
          , stdinPort cfg
          , heartbeatPort cfg
          , controlPort cfg
          ]
      = Left $ ConfigValidationError "All ports must be positive integers"
      | signatureScheme cfg `elem` ["", "hmac-sha256"] = Right cfg
      | otherwise = Left $ ConfigValidationError "Unsupported signature scheme"

-- | Run the kernel within bracketed ZeroMQ sockets while executing the provided action.
withKernel :: KernelProcessConfig -> IO a -> IO a
withKernel cfg action = do
  tempRoot <- getTemporaryDirectory
  let runtimeTemp = tempRoot </> "hsjupyter"
  createDirectoryIfMissing True runtimeTemp
  let budget = ResourceBudget
        { rbCpuTimeout = 10
        , rbMemoryLimit = 512 * 1024 * 1024
        , rbTempDirectory = runtimeTemp
        , rbMaxStreamBytes = fromIntegral payloadLimitBytes
        }
      queueCapacity = 16
  withRuntimeManager budget queueCapacity $ \manager ->
    Z.withContext $ \ctx ->
      Z.withSocket ctx Z.Router $ \shell ->
      Z.withSocket ctx Z.Router $ \control ->
      Z.withSocket ctx Z.Router $ \stdinSock ->
      Z.withSocket ctx Z.Pub    $ \iopub ->
      Z.withSocket ctx Z.Rep    $ \heartbeat -> do
        configureSocket shell
        configureSocket control
        configureSocket stdinSock
        configureSocket iopub
        configureSocket heartbeat
        bindAll shell control stdinSock iopub heartbeat
        bridgeCtx <- mkBridgeContext cfg manager
        runKatipContextT (logEnv bridgeCtx) () "kernel" $
          logFM InfoS "Kernel sockets bound"
        now <- getCurrentTime
        lastBeatRef <- newIORef now
        statusRef <- newIORef Healthy
        shutdownRef <- newIORef False
        let keyBytes = key cfg
            loops =
              [ shellLoop bridgeCtx keyBytes shell iopub shutdownRef
              , controlLoop bridgeCtx keyBytes control shutdownRef
              , heartbeatLoop heartbeat lastBeatRef
              , heartbeatMonitorLoop bridgeCtx lastBeatRef statusRef
              ]
        bracket (traverse async loops) (mapM_ cancel) $ \threads -> do
          mapM_ link threads
          action
  where
    configureSocket sock = Z.setLinger (Z.restrict (0 :: Int)) sock
    bindAll s c i p h = do
      bindOrDie "shell"     s (endpoint (shellPort cfg))
      bindOrDie "control"   c (endpoint (controlPort cfg))
      bindOrDie "stdin"     i (endpoint (stdinPort cfg))
      bindOrDie "iopub"     p (endpoint (iopubPort cfg))
      bindOrDie "heartbeat" h (endpoint (heartbeatPort cfg))
    endpoint port = T.unpack (transport cfg) <> "://" <> T.unpack (ipAddress cfg) <> ":" <> show port
    bindOrDie label sock ep = do
      result <- try (Z.bind sock ep) :: IO (Either Z.ZMQError ())
      case result of
        Right _ -> pure ()
        Left err -> die $ "[hsjupyter] failed to bind " <> label <> " socket at " <> ep <> " : " <> displayException err

-- | Run the kernel loops indefinitely (CTRL+C to exit).
runKernel :: KernelProcessConfig -> IO ()
runKernel cfg = withKernel cfg (forever (threadDelay 1000000))

-- Internal loops ------------------------------------------------------------

shellLoop :: BridgeContext -> BS.ByteString -> Z.Socket Z.Router -> Z.Socket Z.Pub -> IORef Bool -> IO ()
shellLoop ctx keyBytes shellSock iopubSock _shutdownRef = forever $ runKatipContextT (logEnv ctx) () "shell" $ do
  logFM InfoS "Shell loop: waiting for message..."
  frames <- liftIO $ Z.receiveMulti shellSock
  logFM InfoS $ logStr $ "Shell loop: received " <> show (length frames) <> " total frames"
  logFM DebugS $ logStr $ "Frame lengths: " <> show (map BS.length frames)

  case parseEnvelopeFrames frames of
    Left (EnvelopeFrameError err) ->
      logFM ErrorS $ logStr $ "Shell loop: parseEnvelopeFrames failed: " <> T.unpack err
    Right envelope -> do
      let mtype = msgType (envelopeHeader envelope)
          identityCount = length (envelopeIdentities envelope)
      logFM InfoS $ logStr $ "Shell loop: parsed envelope, msg_type=" <> T.unpack mtype <> ", identities=" <> show identityCount
      
      -- Route based on message type
      let valid = verifySignature keyBytes envelope
      unless valid $
        logFM WarningS "Rejected envelope with invalid signature"
      
      result <- if not valid
        then return $ Left SignatureValidationFailed
        else liftIO $ handleRequest ctx (bridgeManager ctx) envelope
      
      case result of
        Left bridgeErr ->
          logFM ErrorS $ logStr $ T.unpack (bridgeErrorMessage bridgeErr)
        Right envelopes -> do
          logFM InfoS $ logStr $ "Shell loop: sending " <> show (length envelopes) <> " reply envelope(s)"
          for_ (zip [0 :: Int ..] envelopes) $ \(idx, env) -> do
            -- Router sockets require at least one identity frame for routing
            -- If identity frames are missing, add a default one to prevent routing failure
            let fixedEnv = if null (envelopeIdentities env)
                          then -- This should never happen with proper Router socket communication
                               -- but we add a default identity to be defensive
                               env { envelopeIdentities = [BS.pack "default"] }
                          else env
            when (null (envelopeIdentities env)) $
              logFM WarningS $ logStr $ "Warning: Reply envelope had no identity frames! Added default identity."
            logFM DebugS $ logStr $ "Envelope identities: " <> show (length (envelopeIdentities fixedEnv)) <> " frame(s)"
            let rendered = renderEnvelopeFrames keyBytes fixedEnv
            if idx == 0
              then do
                logFM InfoS $ logStr $ "Shell loop: sending reply on shell socket (" <> show (length rendered) <> " frames)"
                logFM DebugS $ logStr $ "Frame breakdown: " <> show (length (envelopeIdentities fixedEnv)) <> " identity + 6 payload frames"
                result <- liftIO $ try (sendFrames shellSock rendered) :: KatipContextT IO (Either Z.ZMQError ())
                case result of
                  Left err -> logFM ErrorS $ logStr $ "Failed to send reply on shell socket: " <> displayException err
                  Right () -> logFM DebugS "Successfully sent reply on shell socket"
              else liftIO $ sendIOPub fixedEnv rendered
  where
    sendIOPub env rendered =
      let topic = TE.encodeUtf8 (msgType (envelopeHeader env))
      in sendFrames iopubSock (topic : rendered)

payloadLimitBytes :: Int64
payloadLimitBytes = 1024 * 1024

controlLoop :: BridgeContext -> BS.ByteString -> Z.Socket Z.Router -> IORef Bool -> IO ()
controlLoop ctx _keyBytes controlSock shutdownRef = forever $ runKatipContextT (logEnv ctx) () "control" $ do
  frames <- liftIO $ Z.receiveMulti controlSock
  case parseEnvelopeFrames frames of
    Left (EnvelopeFrameError err) ->
      logFM WarningS $ logStr $ "Malformed control frame: " <> T.unpack err
    Right envelope -> do
      -- TODO: Implement interrupt logic
      logFM InfoS $ logStr $ "Control loop received: " <> show (envelopeHeader envelope)
      when (msgType (envelopeHeader envelope) == "shutdown_request") $
        liftIO $ writeIORef shutdownRef True

heartbeatLoop :: Z.Socket Z.Rep -> IORef UTCTime -> IO ()
heartbeatLoop heartbeatSock lastBeatRef = forever $ do
  msg <- Z.receive heartbeatSock
  Z.send heartbeatSock [] msg
  now <- getCurrentTime
  writeIORef lastBeatRef now

heartbeatMonitorLoop :: BridgeContext -> IORef UTCTime -> IORef HeartbeatStatus -> IO ()
heartbeatMonitorLoop ctx lastBeatRef statusRef = forever $ runKatipContextT (logEnv ctx) () "heartbeat-monitor" $ do
  liftIO $ threadDelay heartbeatPollInterval
  lastBeat <- liftIO $ readIORef lastBeatRef
  now <- liftIO getCurrentTime
  let latency = diffUTCTime now lastBeat
      newStatus = classify latency
  currentStatus <- liftIO $ readIORef statusRef
  when (newStatus /= currentStatus) $ do
    liftIO $ writeIORef statusRef newStatus
    logFM (statusLevel newStatus) (statusMessage latency newStatus)
  where
    classify dt
      | dt < healthyThreshold      = Healthy
      | dt < unresponsiveThreshold = Degraded
      | otherwise                  = Unresponsive
    statusLevel Healthy      = InfoS
    statusLevel Degraded     = WarningS
    statusLevel Unresponsive = ErrorS
    statusMessage dt s = logStr $ "Heartbeat " <> T.pack (show s) <> " (latency " <> T.pack (show dt) <> ")"

healthyThreshold, unresponsiveThreshold :: NominalDiffTime
healthyThreshold = 0.5
unresponsiveThreshold = 5.0

heartbeatPollInterval :: Int
heartbeatPollInterval = 500 * 1000 -- microseconds

sendFrames :: Z.Sender a => Z.Socket a -> [BS.ByteString] -> IO ()
sendFrames sock frames =
  case NE.nonEmpty frames of
    Nothing -> pure ()
    Just ne -> Z.sendMulti sock ne

bridgeErrorMessage :: BridgeError -> Text
bridgeErrorMessage SignatureValidationFailed = "Rejected: signature validation failed"
bridgeErrorMessage (DecodeFailure msg)        = "Rejected: " <> msg
