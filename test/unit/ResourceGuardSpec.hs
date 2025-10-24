{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceGuardSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (evaluate, try, SomeException)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Test.Hspec

import HsJupyter.Runtime.ResourceGuard
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
  )

spec :: Spec
spec = describe "Resource Guards" $ do
  describe "ResourceLimits" $ do
    it "has sensible defaults" $ do
      let limits = defaultResourceLimits
      
      rcMaxCpuSeconds limits `shouldSatisfy` (> 0)
      rcMaxMemoryMB limits `shouldSatisfy` (> 0)
      rcMaxOutputBytes limits `shouldSatisfy` (> 0)
      rcCpuMode limits `shouldBe` CpuWall
      rcMemoryMode limits `shouldBe` MemoryResident

    it "can be customized" $ do
      let limits = ResourceLimits
            { rcMaxCpuSeconds = 5.0
            , rcMaxMemoryMB = 256
            , rcMaxOutputBytes = 1024
            , rcCpuMode = CpuUser
            , rcMemoryMode = MemoryVirtual
            }
      
      rcMaxCpuSeconds limits `shouldBe` 5.0
      rcMaxMemoryMB limits `shouldBe` 256
      rcMaxOutputBytes limits `shouldBe` 1024
      rcCpuMode limits `shouldBe` CpuUser
      rcMemoryMode limits `shouldBe` MemoryVirtual

  describe "ResourceConfig" $ do
    it "can be configured with custom limits" $ do
      let config = ResourceConfig
            { rgLimits = defaultResourceLimits { rcMaxCpuSeconds = 10.0 }
            , rgEnforcement = True
            , rgMonitoringInterval = 0.1
            }
      
      rcMaxCpuSeconds (rgLimits config) `shouldBe` 10.0
      rgEnforcement config `shouldBe` True
      rgMonitoringInterval config `shouldBe` 0.1

    it "can disable enforcement" $ do
      let config = ResourceConfig
            { rgLimits = defaultResourceLimits
            , rgEnforcement = False  
            , rgMonitoringInterval = 1.0
            }
      
      rgEnforcement config `shouldBe` False

  describe "execution time limits" $ do
    it "allows execution within time limit" $ do
      result <- limitExecutionTime 1.0 $ do
        threadDelay 100000  -- 100ms
        return "success"
      
      result `shouldBe` Right "success"

    it "cancels execution exceeding time limit" $ do
      start <- getCurrentTime
      result <- limitExecutionTime 0.1 $ do
        threadDelay 500000  -- 500ms
        return "should not complete"
      
      end <- getCurrentTime
      let elapsed = realToFrac $ diffUTCTime end start
      
      result `shouldSatisfy` isTimeoutViolation
      elapsed `shouldSatisfy` (< 0.3)  -- Should terminate quickly

    it "handles exceptions during timed execution" $ do
      result <- limitExecutionTime 1.0 $ do
        error "test exception"
      
      (result :: Either ResourceViolation String) `shouldSatisfy` isLeft
      
    it "measures execution time accurately" $ do
      start <- getCurrentTime
      result <- limitExecutionTime 0.5 $ do
        threadDelay 200000  -- 200ms
        return ()
      
      end <- getCurrentTime
      let elapsed = realToFrac $ diffUTCTime end start
      
      result `shouldBe` Right ()
      elapsed `shouldSatisfy` (\t -> t >= 0.15 && t <= 0.35)  -- Allow some variance

  describe "memory monitoring" $ do
    it "tracks memory usage during execution" $ do
      result <- monitorMemoryUsage $ do
        -- Allocate some memory
        let bigList = [1..10000] :: [Int]
        _ <- evaluate (sum bigList)
        return "completed"
      
      result `shouldSatisfy` isRight
      case result of
        Right (value, maxMem) -> do
          value `shouldBe` "completed"
          maxMem `shouldSatisfy` (> 0)
        Left _ -> expectationFailure "Should not fail"

    it "reports peak memory usage" $ do
      result <- monitorMemoryUsage $ do
        -- Create increasing memory pressure
        let allocStep n = replicate n (n :: Int)
        let steps = map allocStep [1000, 2000, 3000, 2000, 1000]
        mapM_ (\step -> evaluate (sum step)) steps
        return ()
      
      case result of
        Right (_, maxMem) -> maxMem `shouldSatisfy` (> 0)
        Left _ -> expectationFailure "Should not fail"

    it "handles exceptions during memory monitoring" $ do
      result <- monitorMemoryUsage $ do
        error "memory monitoring test exception"
      
      (result :: Either SomeException (String, Int)) `shouldSatisfy` isLeft

  describe "output truncation" $ do
    it "preserves output within limits" $ do
      let shortOutput = "Short output"
          result = truncateOutput 1000 shortOutput
      
      result `shouldBe` shortOutput

    it "truncates output exceeding limits" $ do
      let longOutput = replicate 2000 'x'
          result = truncateOutput 1000 longOutput
      
      length result `shouldBe` 1000
      result `shouldSatisfy` (not . null)

    it "handles empty output" $ do
      let result = truncateOutput 1000 ""
      result `shouldBe` ""

    it "handles zero limit" $ do
      let result = truncateOutput 0 "some output"
      result `shouldBe` ""

    it "preserves structure for multi-line output" $ do
      let multilineOutput = unlines ["Line 1", "Line 2", "Line 3", "Line 4"]
          result = truncateOutput 20 multilineOutput
      
      length result `shouldBe` 20
      result `shouldSatisfy` (\s -> '\n' `elem` s)  -- Should preserve some structure

  describe "integrated resource checking" $ do
    it "checks all resource limits together" $ do
      let limits = defaultResourceLimits { rcMaxCpuSeconds = 1.0, rcMaxMemoryMB = 100 }
      
      result <- checkResourceLimits limits $ do
        threadDelay 100000  -- 100ms - within CPU limit
        return "success"
      
      result `shouldBe` Right "success"

    it "detects CPU limit violations" $ do
      let limits = defaultResourceLimits { rcMaxCpuSeconds = 0.1 }
      
      result <- checkResourceLimits limits $ do
        threadDelay 300000  -- 300ms - exceeds CPU limit
        return "should not complete"
      
      result `shouldSatisfy` isTimeoutViolation

    it "combines time and memory monitoring" $ do
      let limits = defaultResourceLimits 
            { rcMaxCpuSeconds = 1.0
            , rcMaxMemoryMB = 200
            }
      
      result <- checkResourceLimits limits $ do
        -- Moderate memory allocation within time limit
        let moderateList = [1..5000] :: [Int]
        _ <- evaluate (sum moderateList)
        threadDelay 200000  -- 200ms
        return "completed"
      
      result `shouldBe` Right "completed"

  describe "ResourceGuard integration" $ do
    it "creates guard with default configuration" $ do
      result <- withResourceGuard defaultResourceLimits $ \_ -> do
        return "guard created successfully"
      
      result `shouldBe` "guard created successfully"

    it "enforces limits through guard" $ do
      let limits = defaultResourceLimits { rcMaxCpuSeconds = 0.1 }
      
      result <- (try :: IO String -> IO (Either SomeException String)) $ withResourceGuard limits $ \_ -> do
        threadDelay 300000  -- Should be terminated
        return "should not complete"
      
      case result of
        Left _ex -> return ()  -- Expected exception
        Right _ -> expectationFailure "Should have been terminated"

    it "allows normal execution through guard" $ do
      result <- withResourceGuard defaultResourceLimits $ \_ -> do
        threadDelay 50000  -- 50ms - should be fine
        return "normal execution"
      
      result `shouldBe` "normal execution"

  describe "ResourceViolation types" $ do
    it "categorizes timeout violations" $ do
      let violation = TimeoutViolation 5.0 0.1
      
      case violation of
        TimeoutViolation elapsed limit -> do
          elapsed `shouldBe` 5.0
          limit `shouldBe` 0.1
        _ -> expectationFailure "Should be TimeoutViolation"

    it "categorizes memory violations" $ do
      let violation = MemoryViolation 512 256
      
      case violation of
        MemoryViolation used limit -> do
          used `shouldBe` 512
          limit `shouldBe` 256
        _ -> expectationFailure "Should be MemoryViolation"

    it "categorizes output violations" $ do
      let violation = OutputViolation 2048 1024
      
      case violation of
        OutputViolation size limit -> do
          size `shouldBe` 2048
          limit `shouldBe` 1024
        _ -> expectationFailure "Should be OutputViolation"

-- Helper functions for testing

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight = not . isLeft

isTimeoutViolation :: Either ResourceViolation a -> Bool
isTimeoutViolation (Left (TimeoutViolation _ _)) = True
isTimeoutViolation _ = False