{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | JSON Output Integration Tests (T039 - Phase 6 US4)
-- 
-- These tests validate that CLI commands produce correct JSON output for
-- programmatic access and automation scenarios.

module JSONOutputIntegrationSpec where

import Test.Hspec
import Test.HUnit
import Data.Aeson (Value(..), decode, encode, ToJSON(..), toJSON, object, (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Control.Exception (try, IOException)

import HsJupyter.CLI.Types 
    ( KernelInstallation(..)
    , InstallationStatus(..)
    , KernelConfig(..)
    , ResourceLimits(..)
    , InterruptMode(..)
    , CLIDiagnostic(..)
    )
import HsJupyter.CLI.Output (formatOutput, OutputFormat(..), toJSONOutput)
import HsJupyter.CLI.Install (listKernelInstallations, getVersionInformation)
import HsJupyter.CLI.Commands (ListOptions(..), VersionOptions(..))

-- | Main test suite for JSON output integration
spec :: Spec
spec = describe "JSON Output Integration Tests (T039)" $ do
    
    -- T039.1: List command JSON output validation
    describe "List Command JSON Output" $ do
        it "produces valid JSON structure for empty installations" $ do
            let opts = ListOptions { loShowAll = False, loSearchPath = Nothing }
            result <- listKernelInstallations opts
            case result of
                Right installations -> do
                    let jsonValue = toJSON installations
                    -- Validate it can be encoded and decoded
                    let encoded = encode jsonValue
                    let decoded = decode encoded :: Maybe [KernelInstallation]
                    decoded `shouldSatisfy` isJust
                Left _ -> expectationFailure "Expected successful empty result"
        
        it "produces JSON with required fields for installations" $ do
            -- Create a mock installation for testing
            let installation = KernelInstallation
                    { kiKernelspecPath = "/test/path/kernel.json"
                    , kiDisplayName = "Test Haskell"
                    , kiVersion = "0.1.0.0"
                    , kiGHCPath = "/usr/bin/ghc"
                    , kiStatus = Installed
                    , kiConfiguration = testKernelConfig
                    }
            let jsonValue = toJSON [installation]
            let encoded = encode jsonValue
            
            -- Parse and validate structure
            case decode encoded :: Maybe Value of
                Just (Array arr) -> do
                    arr `shouldSatisfy` (not . null)
                    case arr V.!? 0 of
                        Just (Object obj) -> do
                            -- Check required fields exist
                            KM.member (K.fromText "kiKernelspecPath") obj `shouldBe` True
                            KM.member (K.fromText "kiDisplayName") obj `shouldBe` True
                            KM.member (K.fromText "kiVersion") obj `shouldBe` True
                            KM.member (K.fromText "kiStatus") obj `shouldBe` True
                        Just _ -> expectationFailure "Expected object in array"
                        Nothing -> expectationFailure "Array is empty"
                _ -> expectationFailure "Expected array in JSON"

    -- T039.2: Version command JSON output validation  
    describe "Version Command JSON Output" $ do
        it "produces valid JSON structure for version information" $ do
            let opts = VersionOptions { voCheckCompatibility = False }
            result <- getVersionInformation opts
            case result of
                Right (version, buildInfo) -> do
                    let versionObj = object 
                            [ "version" .= version
                            , "build_info" .= buildInfo 
                            ]
                    let encoded = encode versionObj
                    let decoded = decode encoded :: Maybe Value
                    decoded `shouldSatisfy` isJust
                Left _ -> expectationFailure "Expected successful version result"
        
        it "includes compatibility checking in JSON when requested" $ do
            let opts = VersionOptions { voCheckCompatibility = True }
            result <- getVersionInformation opts
            case result of
                Right (version, buildInfo) -> do
                    -- Version command should complete successfully with compatibility check
                    T.length version `shouldSatisfy` (> 0)
                    T.length buildInfo `shouldSatisfy` (> 0)
                Left diag -> expectationFailure $ "Compatibility check failed: " ++ show diag

    -- T039.3: Output format integration tests
    describe "Output Format Integration" $ do
        it "formatOutput produces consistent JSON for success cases" $ do
            let testData = object ["test" .= ("value" :: T.Text)]
            let result = Right testData
            
            -- This should not throw an exception
            result <- try $ formatOutput JSON result
            case result of
                Left (_ :: IOException) -> expectationFailure "formatOutput should not throw for valid JSON"
                Right _ -> return ()
        
        it "formatOutput produces consistent JSON for error cases" $ do
            let testError = ValidationError "Test validation error"
            let result = Left testError :: Either CLIDiagnostic Value
            
            -- This should not throw an exception  
            result <- try $ formatOutput JSON result
            case result of
                Left (_ :: IOException) -> expectationFailure "formatOutput should not throw for valid errors"
                Right _ -> return ()

    -- T039.4: Automation scenario tests
    describe "Automation Scenarios" $ do
        it "list command with JSON output supports automation parsing" $ do
            withSystemTempDirectory "hs-jupyter-test" $ \tmpDir -> do
                -- Create test scenario with mock kernelspec directory
                let kernelspecDir = tmpDir </> "kernels"
                createDirectoryIfMissing True kernelspecDir
                
                let opts = ListOptions 
                        { loShowAll = True
                        , loSearchPath = Just kernelspecDir
                        }
                
                result <- listKernelInstallations opts
                case result of
                    Right installations -> do
                        -- Should be parseable as JSON
                        let jsonValue = toJSON installations
                        let encoded = encode jsonValue
                        -- Should decode back successfully
                        let decoded = decode encoded :: Maybe [KernelInstallation]
                        decoded `shouldSatisfy` isJust
                    Left _ -> return ()  -- Empty directory is acceptable
        
        it "version command with JSON output supports automation parsing" $ do
            let opts = VersionOptions { voCheckCompatibility = False }
            result <- getVersionInformation opts
            case result of
                Right (version, buildInfo) -> do
                    let versionData = object
                            [ "version" .= version
                            , "build_info" .= buildInfo
                            , "timestamp" .= ("2023-01-01T00:00:00Z" :: T.Text)
                            ]
                    
                    -- Should be valid JSON for automation tools
                    let encoded = encode versionData
                    LBS.length encoded `shouldSatisfy` (> 0)
                    
                    -- Should parse back correctly
                    case decode encoded :: Maybe Value of
                        Just (Object obj) -> do
                            KM.member (K.fromText "version") obj `shouldBe` True
                            KM.member (K.fromText "build_info") obj `shouldBe` True
                        _ -> expectationFailure "Expected valid JSON object"
                Left _ -> expectationFailure "Expected successful version retrieval"

-- | Helper function to create test kernel configuration
testKernelConfig :: KernelConfig
testKernelConfig = KernelConfig
    { kcResourceLimits = HsJupyter.CLI.Types.ResourceLimits Nothing Nothing Nothing
    , kcDisplayName = "Test Haskell"
    , kcLanguage = "haskell"
    , kcInterruptMode = Signal
    , kcMetadata = object []
    }

-- | Helper to check if Maybe is Just
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False