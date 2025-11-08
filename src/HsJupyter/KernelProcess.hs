module HsJupyter.KernelProcess
  ( module HsJupyter.Kernel.Types
  , loadKernelProcessConfig
  , withKernel
  , runKernel
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket, displayException, try)
import Control.Monad (forever, when)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory)
import System.Exit (die)
import System.FilePath ((</>))
import qualified System.ZMQ4 as Z

import HsJupyter.Bridge.JupyterBridge
  ( BridgeContext(..)
  , BridgeError(..)
  , handleExecuteOnce
  , handleKernelInfo
  , handleInterrupt
  , logBridgeEvent
  , mkBridgeContext
  )
import HsJupyter.Runtime.Manager
  ( withRuntimeManager
  )
import HsJupyter.Runtime.SessionState (ResourceBudget(..))
import HsJupyter.Bridge.Protocol.Codec
  ( EnvelopeFrameError(..)
  , parseEnvelopeFrames
  , renderEnvelopeFrames
  )
import HsJupyter.Bridge.Protocol.Envelope
  ( ProtocolEnvelope(..)
  , MessageHeader(..)
  , envelopeContent
  , envelopeHeader
  , msgType
  )
import HsJupyter.Bridge.HeartbeatThread (HeartbeatStatus(..))
import HsJupyter.Kernel.Types
import HsJupyter.Runtime.Manager (withRuntimeManager)
import HsJupyter.Runtime.SessionState (ResourceBudget(..))



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
        logBridgeEvent cfg (logLevel cfg) LogInfo "Kernel sockets bound"
        now <- getCurrentTime
        lastBeatRef <- newIORef now
        statusRef <- newIORef Healthy
        let keyBytes = key cfg
            loops =
              [ shellLoop bridgeCtx keyBytes shell iopub
              , controlLoop bridgeCtx keyBytes control
              , heartbeatLoop heartbeat lastBeatRef
              , heartbeatMonitorLoop cfg lastBeatRef statusRef
              ]
        bracket (traverse async loops) (mapM_ cancel) $ \_ -> action
  where
    configureSocket sock = do
      Z.setLinger (Z.restrict (0 :: Int)) sock
      pure ()

    bindAll shell control stdinSock iopub heartbeat = do
      bindOrDie "shell"     shell     (endpoint (shellPort cfg))
      bindOrDie "control"   control   (endpoint (controlPort cfg))
      bindOrDie "stdin"     stdinSock (endpoint (stdinPort cfg))
      bindOrDie "iopub"     iopub     (endpoint (iopubPort cfg))
      bindOrDie "heartbeat" heartbeat (endpoint (heartbeatPort cfg))

    endpoint port = T.unpack (transport cfg) <> "://" <> T.unpack (ipAddress cfg) <> ":" <> show port

    bindOrDie label sock ep = do
      result <- (try (Z.bind sock ep) :: IO (Either Z.ZMQError ()))
      case result of
        Right _ -> pure ()
        Left err -> do
          let detail = "[hsjupyter] failed to bind " <> label <> " socket at " <> ep <> " : " <> displayException err
          putStrLn detail
          die "[hsjupyter] kernel startup aborted"

-- | Run the kernel loops indefinitely (CTRL+C to exit).
runKernel :: KernelProcessConfig -> IO ()
runKernel cfg = withKernel cfg (forever (threadDelay 1000000))

-- Internal loops ------------------------------------------------------------

shellLoop :: BridgeContext -> BS.ByteString -> Z.Socket Z.Router -> Z.Socket Z.Pub -> IO ()
shellLoop ctx keyBytes shellSock iopubSock = forever $ do
  logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogInfo "Shell loop: waiting for message..."
  frames <- Z.receiveMulti shellSock
  logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogInfo 
    ("Shell loop: received " <> T.pack (show (length frames)) <> " frames")
  case parseEnvelopeFrames frames of
    Left (EnvelopeFrameError err) -> do
      logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn ("Malformed shell frame: " <> err)
    Right envelope -> do
      let payloadBytes = LBS.length (Aeson.encode (envelopeContent envelope))
          mtype = msgType (envelopeHeader envelope)
      logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogInfo 
        ("Shell loop: parsed envelope, msg_type=" <> mtype)
      when (payloadBytes > payloadLimitBytes) $
        logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn
          ("Received payload exceeding 1MB (" <> T.pack (show payloadBytes) <> " bytes)")
      
      -- Route based on message type
      result <- case mtype of
        "kernel_info_request" -> do
          logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogInfo "Shell loop: calling handleKernelInfo"
          infoResult <- handleKernelInfo ctx envelope
          case infoResult of
            Left err -> do
              logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogError "Shell loop: handleKernelInfo failed"
              pure $ Left err
            Right reply -> do
              logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogInfo "Shell loop: handleKernelInfo succeeded"
              pure $ Right [reply]
        "execute_request" -> handleExecuteOnce ctx envelope
        _ -> do
          logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn 
            ("Unsupported message type on shell: " <> mtype)
          pure $ Left (DecodeFailure ("Unsupported message type: " <> mtype))
      
      case result of
        Left bridgeErr ->
          logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogError (bridgeErrorMessage bridgeErr)
        Right envelopes -> do
          logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogInfo 
            ("Shell loop: sending " <> T.pack (show (length envelopes)) <> " reply envelope(s)")
          for_ (zip [0 :: Int ..] envelopes) $ \(idx, env) -> do
            let rendered = renderEnvelopeFrames keyBytes env
            if idx == 0
              then do
                logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogInfo 
                  ("Shell loop: sending reply on shell socket (" <> T.pack (show (length rendered)) <> " frames)")
                sendFrames shellSock rendered
              else sendIOPub env rendered
  where
    sendIOPub env rendered =
      let topic = TE.encodeUtf8 (msgType (envelopeHeader env))
      in sendFrames iopubSock (topic : rendered)


payloadLimitBytes :: Int64
payloadLimitBytes = 1024 * 1024
controlLoop :: BridgeContext -> BS.ByteString -> Z.Socket Z.Router -> IO ()
controlLoop ctx keyBytes controlSock = forever $ do
  frames <- Z.receiveMulti controlSock
  case parseEnvelopeFrames frames of
    Left (EnvelopeFrameError err) ->
      logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn ("Malformed control frame: " <> err)
    Right envelope -> do
      reply <- handleInterrupt ctx envelope
      let rendered = renderEnvelopeFrames keyBytes reply
      sendFrames controlSock rendered

heartbeatLoop :: Z.Socket Z.Rep -> IORef UTCTime -> IO ()
heartbeatLoop heartbeatSock lastBeatRef = forever $ do
  msg <- Z.receive heartbeatSock
  Z.send heartbeatSock [] msg
  now <- getCurrentTime
  writeIORef lastBeatRef now

heartbeatMonitorLoop :: KernelProcessConfig -> IORef UTCTime -> IORef HeartbeatStatus -> IO ()
heartbeatMonitorLoop cfg lastBeatRef statusRef = forever $ do
  threadDelay heartbeatPollInterval
  lastBeat <- readIORef lastBeatRef
  now <- getCurrentTime
  let latency = diffUTCTime now lastBeat
      newStatus = classify latency
  currentStatus <- readIORef statusRef
  when (newStatus /= currentStatus) $ do
    writeIORef statusRef newStatus
    logBridgeEvent cfg (logLevel cfg) (statusLevel newStatus) (statusMessage latency newStatus)
  where
    classify dt
      | dt < healthyThreshold      = Healthy
      | dt < unresponsiveThreshold = Degraded
      | otherwise                  = Unresponsive

    statusLevel Healthy      = LogInfo
    statusLevel Degraded     = LogWarn
    statusLevel Unresponsive = LogError

    statusMessage dt Healthy      = "Heartbeat healthy (latency " <> T.pack (show dt) <> ")"
    statusMessage dt Degraded     = "Heartbeat degraded (last probe " <> T.pack (show dt) <> "s ago)"
    statusMessage dt Unresponsive = "Heartbeat unresponsive (last probe " <> T.pack (show dt) <> "s ago)"

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
