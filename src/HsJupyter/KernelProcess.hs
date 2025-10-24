module HsJupyter.KernelProcess
  ( module HsJupyter.Kernel.Types
  , loadKernelProcessConfig
  , withKernel
  , runKernel
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (doesFileExist)
import qualified System.ZMQ4 as Z

import HsJupyter.Bridge.JupyterBridge
  ( BridgeContext(..)
  , BridgeError(..)
  , handleExecuteOnce
  , handleInterrupt
  , logBridgeEvent
  , mkBridgeContext
  )
import HsJupyter.Bridge.Protocol.Codec
  ( EnvelopeFrameError(..)
  , parseEnvelopeFrames
  , renderEnvelopeFrames
  )
import HsJupyter.Kernel.Types



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
withKernel cfg action =
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
      bridgeCtx <- mkBridgeContext cfg
      logBridgeEvent cfg (logLevel cfg) LogInfo "Kernel sockets bound"
      let keyBytes = key cfg
          loops =
            [ shellLoop bridgeCtx keyBytes shell iopub
            , controlLoop bridgeCtx keyBytes control
            , heartbeatLoop heartbeat
            ]
      bracket (traverse async loops) (mapM_ cancel) $ \_ -> action
  where
    configureSocket sock = do
      Z.setLinger (Z.restrict (0 :: Int)) sock
      pure ()

    bindAll shell control stdinSock iopub heartbeat = do
      Z.bind shell     (endpoint (shellPort cfg))
      Z.bind control   (endpoint (controlPort cfg))
      Z.bind stdinSock (endpoint (stdinPort cfg))
      Z.bind iopub     (endpoint (iopubPort cfg))
      Z.bind heartbeat (endpoint (heartbeatPort cfg))

    endpoint port = T.unpack (transport cfg) <> "://" <> T.unpack (ipAddress cfg) <> ":" <> show port

-- | Run the kernel loops indefinitely (CTRL+C to exit).
runKernel :: KernelProcessConfig -> IO ()
runKernel cfg = withKernel cfg (forever (threadDelay 1000000))

-- Internal loops ------------------------------------------------------------

shellLoop :: BridgeContext -> BS.ByteString -> Z.Socket Z.Router -> Z.Socket Z.Pub -> IO ()
shellLoop ctx keyBytes shellSock iopubSock = forever $ do
  frames <- Z.receiveMulti shellSock
  case parseEnvelopeFrames frames of
    Left (EnvelopeFrameError err) ->
      logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn ("Malformed shell frame: " <> err)
    Right envelope -> do
      result <- handleExecuteOnce ctx envelope
      case result of
        Left bridgeErr ->
          logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogError (bridgeErrorMessage bridgeErr)
        Right envelopes ->
          for_ (zip [0 :: Int ..] envelopes) $ \(idx, env) -> do
            let rendered = renderEnvelopeFrames keyBytes env
            if idx == 0
              then sendFrames shellSock rendered
              else sendIOPub env rendered
  where
    sendIOPub _env rendered = sendFrames iopubSock rendered

controlLoop :: BridgeContext -> BS.ByteString -> Z.Socket Z.Router -> IO ()
controlLoop ctx keyBytes controlSock = forever $ do
  frames <- Z.receiveMulti controlSock
  case parseEnvelopeFrames frames of
    Left (EnvelopeFrameError err) ->
      logBridgeEvent (bridgeConfig ctx) (logLevel (bridgeConfig ctx)) LogWarn ("Malformed control frame: " <> err)
    Right envelope -> do
      let reply = handleInterrupt ctx envelope
          rendered = renderEnvelopeFrames keyBytes reply
      sendFrames controlSock rendered

heartbeatLoop :: Z.Socket Z.Rep -> IO ()
heartbeatLoop heartbeatSock = forever $ do
  msg <- Z.receive heartbeatSock
  Z.send heartbeatSock [] msg

sendFrames :: Z.Sender a => Z.Socket a -> [BS.ByteString] -> IO ()
sendFrames sock frames =
  case NE.nonEmpty frames of
    Nothing -> pure ()
    Just ne -> Z.sendMulti sock ne

bridgeErrorMessage :: BridgeError -> Text
bridgeErrorMessage SignatureValidationFailed = "Rejected: signature validation failed"
bridgeErrorMessage (DecodeFailure msg)        = "Rejected: " <> msg

