{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : HsJupyter.Bridge.JupyterBridge
Description : High-level Jupyter protocol bridge handlers
Copyright   : (c) HsJupyter Contributors 2024-2025
License     : MIT
Maintainer  : dev@hsjupyter.org
Stability   : stable

This module provides the bridge between ZeroMQ protocol messages and
the runtime execution system. It handles message validation, routing,
and response generation for all Jupyter kernel protocol message types.

Key responsibilities:
- HMAC signature verification for incoming messages
- Routing execute requests to the runtime manager
- Generating protocol-compliant replies
- Maintaining bridge-level telemetry (rejected message count)
-}

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
import qualified Katip as K
import           System.IO (stdout)

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

-- | Convert our LogLevel to Katip's Severity.
-- This mapping ensures that the log level configured via --log-level flag
-- or HSJUPYTER_LOG_LEVEL environment variable is properly respected.
-- Fix for: Kernel was ignoring log level configuration and defaulting to warnings only.
toKatipSeverity :: LogLevel -> K.Severity
toKatipSeverity LogDebug = K.DebugS
toKatipSeverity LogInfo  = K.InfoS
toKatipSeverity LogWarn  = K.WarningS
toKatipSeverity LogError = K.ErrorS

-- | Create bridge context with properly configured logging.
-- Creates a Katip LogEnv with the log level from KernelProcessConfig,
-- ensuring shell loop and other kernel components log at the configured level.
mkBridgeContext :: KernelProcessConfig -> RuntimeManager -> IO BridgeContext
mkBridgeContext cfg manager = do
  rejectedVar <- newIORef 0
  -- Create scribe that filters messages based on configured log level
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem (toKatipSeverity (logLevel cfg))) K.V2
  le <- K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<< K.initLogEnv "hs-jupyter" "kernel"
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
