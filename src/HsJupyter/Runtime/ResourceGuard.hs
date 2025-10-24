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

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, catch, throwIO, try)
import Control.Monad (forever, when)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Stats (getRTSStats, RTSStats(..))

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
  
  let guard = ResourceGuard config startTime stopFlag Nothing
  
  if rgEnforcement config
    then do
      -- Start monitoring thread
      monitorThread <- forkIO $ monitorResources guard
      let guardWithMonitor = guard { rgMonitorThread = Just monitorThread }
      
      bracket
        (return guardWithMonitor)
        (\g -> do
          atomically $ writeTVar stopFlag True
          case rgMonitorThread g of
            Just tid -> killThread tid
            Nothing -> return ())
        action
    else action guard

-- | Monitor resource usage in background thread
monitorResources :: ResourceGuard -> IO ()
monitorResources guard = do
  let config = rgConfig guard
      limits = rgLimits config
      interval = rgMonitoringInterval config
  
  catch (forever $ do
    shouldStop <- readTVarIO (rgStopFlag guard)
    when shouldStop $ return ()
    
    -- Check time limit
    now <- getCurrentTime
    let elapsed = realToFrac $ diffUTCTime now (rgStartTime guard)
    when (elapsed > rcMaxCpuSeconds limits) $ do
      throwIO $ TimeoutViolation elapsed (rcMaxCpuSeconds limits)
    
    -- Check memory limit (simplified)
    stats <- getRTSStats
    let memUsedBytes = fromIntegral $ allocated_bytes stats
        memUsedMB = memUsedBytes `div` (1024 * 1024)
    when (memUsedMB > rcMaxMemoryMB limits) $ do
      throwIO $ MemoryViolation memUsedMB (rcMaxMemoryMB limits)
    
    -- Sleep for monitoring interval
    threadDelay $ round (interval * 1000000)
    ) $ \(_ :: SomeException) -> do
      -- Monitor thread caught exception, stop monitoring
      atomically $ writeTVar (rgStopFlag guard) True

-- | Check resource limits for an action
checkResourceLimits :: ResourceLimits -> IO a -> IO (Either ResourceViolation a)
checkResourceLimits limits action = do
  result <- try $ withResourceGuard limits $ \_ -> action
  case result of
    Left violation -> return $ Left violation
    Right value -> return $ Right value

-- | Limit execution time of an action
limitExecutionTime :: Double -> IO a -> IO (Either ResourceViolation a)
limitExecutionTime timeoutSecs action = do
  result <- race 
    (do threadDelay $ round (timeoutSecs * 1000000)
        return $ TimeoutViolation timeoutSecs timeoutSecs)
    (Right <$> action)
  
  case result of
    Left violation -> return $ Left violation
    Right (Right value) -> return $ Right value
    Right (Left _) -> return $ Left $ TimeoutViolation timeoutSecs timeoutSecs

-- | Monitor memory usage during action execution
monitorMemoryUsage :: IO a -> IO (Either SomeException (a, Int))
monitorMemoryUsage action = do
  startStats <- getRTSStats
  let startMem = allocated_bytes startStats
  
  result <- try action
  
  endStats <- getRTSStats
  let endMem = allocated_bytes endStats
      usedMB = fromIntegral (endMem - startMem) `div` (1024 * 1024)
  
  case result of
    Left ex -> return $ Left ex
    Right value -> return $ Right (value, usedMB)

-- | Truncate output to specified byte limit
truncateOutput :: Int -> String -> String
truncateOutput limit output
  | limit <= 0 = ""
  | length output <= limit = output
  | otherwise = take limit output