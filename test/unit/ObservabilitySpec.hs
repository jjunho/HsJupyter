{-# LANGUAGE OverloadedStrings #-}

module ObservabilitySpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Test.Hspec

import HsJupyter.Bridge.HeartbeatThread
  ( HeartbeatSnapshot(..)
  , HeartbeatStatus(..)
  , startHeartbeatThread
  )
import HsJupyter.KernelProcess (LogLevel(..), shouldLog)

spec :: Spec
spec = describe "Observability" $ do
  it "classifies heartbeat latency as healthy for sub-500ms interval" $ do
    mv <- newEmptyMVar
    stopAction <- startHeartbeatThread LogInfo 100000 $ \snapshot ->
      tryPutMVar mv snapshot >> pure ()
    snapshot <- takeMVar mv
    snapshotStatus snapshot `shouldBe` Healthy
    stopAction

  it "enforces log level gating" $ do
    shouldLog LogInfo LogDebug `shouldBe` True
    shouldLog LogDebug LogInfo `shouldBe` False
