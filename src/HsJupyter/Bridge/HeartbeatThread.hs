module HsJupyter.Bridge.HeartbeatThread
  ( HeartbeatStatus(..)
  , HeartbeatSnapshot(..)
  , startHeartbeatThread
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
  ( atomically
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import HsJupyter.Kernel.Types (LogLevel(..))

-- | Discrete heartbeat states exposed for observability.
data HeartbeatStatus
  = Healthy
  | Degraded
  | Unresponsive
  deriving (Eq, Show)

-- | Snapshot delivered to callback listeners.
data HeartbeatSnapshot = HeartbeatSnapshot
  { snapshotStatus  :: HeartbeatStatus
  , snapshotLatency :: NominalDiffTime
  } deriving (Eq, Show)

-- | Spin up a lightweight heartbeat responder that periodically updates metrics.
-- The supplied callback receives snapshots every interval (in microseconds).
startHeartbeatThread
  :: LogLevel
  -> Int                  -- ^ Interval in microseconds
  -> (HeartbeatSnapshot -> IO ())
  -> IO (IO ())           -- ^ Returns action to stop the thread
startHeartbeatThread _ interval callback = do
  statusVar <- newTVarIO Healthy
  latencyVar <- newTVarIO 0
  tid <- forkIO $ loop statusVar latencyVar
  pure (killThread tid)
  where
    loop statusVar latencyVar = do
      start <- getCurrentTime
      threadDelay interval
      end <- getCurrentTime
      let latency = diffUTCTime end start
          newStatus = classify latency
      atomically $ do
        writeTVar latencyVar latency
        writeTVar statusVar newStatus
      snapshot <- HeartbeatSnapshot <$> readTVarIO statusVar <*> readTVarIO latencyVar
      callback snapshot
      loop statusVar latencyVar

    classify :: NominalDiffTime -> HeartbeatStatus
    classify latency
      | latency <= 0.5 = Healthy
      | latency <= 5.0 = Degraded
      | otherwise      = Unresponsive
