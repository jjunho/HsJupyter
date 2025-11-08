{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsJupyter.Runtime.ResourceGuard
  ( ResourceGuard
  , ResourceLimits(..)
  , ResourceConfig(..)
  , CpuLimitMode(..)
  , MemoryLimitMode(..)
  , ResourceViolation(..)
  , withResourceGuard
  , checkResourceLimits
  , limitExecutionTime
  , monitorMemoryUsage
  , truncateOutput
  , defaultResourceLimits
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception
  ( AsyncException (..)
  , Exception
  , SomeException
  , bracket
  , catch
  , displayException
  , throwIO
  , throwTo
  , try
  )
import Control.Monad (unless, when)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Stats (GCDetails(..), RTSStats(..), getRTSStats, getRTSStatsEnabled)

-- | CPU limit measurement modes
data CpuLimitMode 
  = CpuWall      -- ^ Wall clock time
  | CpuUser      -- ^ User CPU time  
  | CpuSystem    -- ^ System CPU time
  deriving (Show, Eq)

-- | Memory limit measurement modes
data MemoryLimitMode
  = MemoryResident   -- ^ Resident set size (RSS)
  | MemoryVirtual    -- ^ Virtual memory size
  deriving (Show, Eq)

-- | Resource limit configuration
data ResourceLimits = ResourceLimits
  { rcMaxCpuSeconds   :: Double           -- ^ Maximum CPU time in seconds
  , rcMaxMemoryMB     :: Int              -- ^ Maximum memory in megabytes
  , rcMaxOutputBytes  :: Int              -- ^ Maximum output size in bytes
  , rcCpuMode         :: CpuLimitMode     -- ^ How to measure CPU usage
  , rcMemoryMode      :: MemoryLimitMode  -- ^ How to measure memory usage
  } deriving (Show, Eq)

-- | Resource guard configuration
data ResourceConfig = ResourceConfig
  { rgLimits              :: ResourceLimits  -- ^ Resource limits to enforce
  , rgEnforcement         :: Bool            -- ^ Whether to enforce limits
  , rgMonitoringInterval  :: Double          -- ^ Monitoring interval in seconds
  } deriving (Show, Eq)

-- | Types of resource violations
data ResourceViolation
  = TimeoutViolation Double Double    -- ^ (elapsed, limit) in seconds
  | MemoryViolation Int Int          -- ^ (used, limit) in MB  
  | OutputViolation Int Int          -- ^ (size, limit) in bytes
  | ExecutionError String            -- ^ Underlying execution raised exception
  deriving (Show, Eq)

instance Exception ResourceViolation

-- | Resource guard handle
data ResourceGuard = ResourceGuard
  { rgConfig      :: ResourceConfig
  , rgStartTime   :: UTCTime
  , rgStopFlag    :: TVar Bool
  , rgMonitorThread :: Maybe ThreadId
  }

-- | Default resource limits (generous for development)
defaultResourceLimits :: ResourceLimits
defaultResourceLimits = ResourceLimits
  { rcMaxCpuSeconds = 30.0      -- 30 second timeout
  , rcMaxMemoryMB = 512         -- 512 MB memory limit
  , rcMaxOutputBytes = 1048576  -- 1 MB output limit
  , rcCpuMode = CpuWall
  , rcMemoryMode = MemoryResident
  }

-- | Execute action with resource guard monitoring
withResourceGuard :: ResourceLimits -> (ResourceGuard -> IO a) -> IO a
withResourceGuard limits action = do
  let config = ResourceConfig limits True 0.1  -- 100ms monitoring interval

  startTime <- getCurrentTime
  stopFlag <- newTVarIO False
  actionThread <- myThreadId

  let baseGuard = ResourceGuard config startTime stopFlag Nothing

  if rgEnforcement config
    then do
      monitorThread <- forkIO $ monitorResources baseGuard actionThread
      let guardWithMonitor = baseGuard { rgMonitorThread = Just monitorThread }

      bracket
        (return guardWithMonitor)
        (\g -> do
          atomically $ writeTVar stopFlag True
          case rgMonitorThread g of
            Just tid -> killThread tid
            Nothing -> pure ())
        action
    else action baseGuard

-- | Monitor resource usage in background thread
monitorResources :: ResourceGuard -> ThreadId -> IO ()
monitorResources guard actionThread = do
  let config = rgConfig guard
      limits = rgLimits config
      intervalMicros = round (rgMonitoringInterval config * 1000000)

      notify violation = do
        shouldNotify <- atomically $ do
          alreadyStopped <- readTVar (rgStopFlag guard)
          if alreadyStopped
            then pure False
            else writeTVar (rgStopFlag guard) True >> pure True
        when shouldNotify $ do
          _ <- try (throwTo actionThread violation) :: IO (Either SomeException ())
          pure ()

  statsEnabled <- getRTSStatsEnabled

  let loop = do
        stop <- readTVarIO (rgStopFlag guard)
        unless stop $ do
          now <- getCurrentTime
          let elapsed = realToFrac $ diffUTCTime now (rgStartTime guard)
          if elapsed > rcMaxCpuSeconds limits
            then notify (TimeoutViolation elapsed (rcMaxCpuSeconds limits))
            else do
              when statsEnabled $ do
                statsResult <- try getRTSStats
                case statsResult of
                  Right stats -> do
                    -- Use max_live_bytes (approx resident/live memory) instead of cumulative allocated_bytes
                    let memUsedBytes = fromIntegral (max_live_bytes stats) :: Integer
                        memUsedMB = fromIntegral (memUsedBytes `div` (1024 * 1024)) :: Int
                    when (memUsedMB > rcMaxMemoryMB limits) $
                      notify (MemoryViolation memUsedMB (rcMaxMemoryMB limits))
                  Left (_ :: SomeException) -> pure ()
              threadDelay intervalMicros
              loop

  loop `catch` \(ae :: AsyncException) ->
    case ae of
      ThreadKilled -> pure ()
      _            -> throwIO ae

-- | Check resource limits for an action
checkResourceLimits :: ResourceLimits -> IO a -> IO (Either ResourceViolation a)
checkResourceLimits limits action = do
  result <- try $ withResourceGuard limits $ \_ -> action
  pure $ case result of
    Left violation -> Left violation
    Right value    -> Right value

-- | Limit execution time of an action
limitExecutionTime :: forall a. Double -> IO a -> IO (Either ResourceViolation a)
limitExecutionTime timeoutSecs action = do
  result <- race
    (do threadDelay $ round (timeoutSecs * 1000000)
        pure (TimeoutViolation timeoutSecs timeoutSecs))
    (try action :: IO (Either SomeException a))

  pure $ case result of
    Left violation      -> Left violation
    Right (Left ex)     -> Left (ExecutionError (displayException ex))
    Right (Right value) -> Right value

-- | Monitor memory usage during action execution
monitorMemoryUsage :: forall a. IO a -> IO (Either SomeException (a, Int))
monitorMemoryUsage action = do
  statsEnabled <- getRTSStatsEnabled
  if not statsEnabled
    then do
      result <- try action :: IO (Either SomeException a)
      pure $ case result of
        Left ex    -> Left ex
        Right value -> Right (value, 0)
    else do
      startStats <- getRTSStats
      let startBytes = fromIntegral (gcdetails_mem_in_use_bytes (gc startStats)) :: Integer

      result <- try action

      endStats <- getRTSStats
      let endBytes = fromIntegral (gcdetails_mem_in_use_bytes (gc endStats)) :: Integer
          usedBytes = max 0 (endBytes - startBytes)
          bytesPerMb :: Double
          bytesPerMb = 1024 * 1024
          rawMb :: Integer
          rawMb = ceiling (fromIntegral usedBytes / bytesPerMb)
          usedMB = if rawMb <= 0 then 1 else fromIntegral rawMb

      pure $ case result of
        Left ex    -> Left ex
        Right value -> Right (value, usedMB)

-- | Truncate output to specified byte limit
truncateOutput :: Int -> String -> String
truncateOutput limit output
  | limit <= 0 = ""
  | length output <= limit = output
  | otherwise = take limit output
