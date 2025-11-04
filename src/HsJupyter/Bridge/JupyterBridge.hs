{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Bridge.JupyterBridge
  ( BridgeContext(..)
  , BridgeError(..)
  , mkBridgeContext
  , handleRequest
  , rejectedCount
  , incrementRejected
  ) where

import           Data.Aeson (Value)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Text (Text)
import qualified Katip as K

import           HsJupyter.Bridge.Protocol.Envelope (ProtocolEnvelope)
import           HsJupyter.Bridge.Types
import           HsJupyter.Kernel.Types
import           HsJupyter.Router (routeRequest)
import           HsJupyter.Runtime.Manager (RuntimeManager)

-- | Operational context shared by bridge handlers.
data BridgeContext = BridgeContext
  { bridgeConfig   :: KernelProcessConfig
  , bridgeManager  :: RuntimeManager
  , bridgeRejected :: IORef Int
  , logEnv         :: K.LogEnv
  }

mkBridgeContext :: KernelProcessConfig -> RuntimeManager -> IO BridgeContext
mkBridgeContext cfg manager = do
  rejectedVar <- newIORef 0
  le <- K.initLogEnv "hs-jupyter" "kernel"
  pure BridgeContext
    { bridgeConfig = cfg
    , bridgeManager = manager
    , bridgeRejected = rejectedVar
    , logEnv = le
    }

rejectedCount :: BridgeContext -> IO Int
rejectedCount = readIORef . bridgeRejected

handleRequest
  :: BridgeContext
  -> RuntimeManager
  -> ProtocolEnvelope Value
  -> IO (Either BridgeError [ProtocolEnvelope Value])
handleRequest ctx manager envelope =
  K.runKatipContextT (logEnv ctx) () "bridge" $
    routeRequest manager envelope

incrementRejected :: BridgeContext -> IO ()
incrementRejected ctx =
  atomicModifyIORef' (bridgeRejected ctx) $ \current -> (current + 1, ())
