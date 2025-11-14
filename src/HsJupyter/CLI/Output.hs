{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : HsJupyter.CLI.Output
Description : JSON and formatted output utilities for CLI commands
Copyright   : (c) HsJupyter Contributors 2024
License     : MIT
Maintainer  : dev@hsjupyter.org
Stability   : experimental

This module provides output formatting for CLI commands, including JSON output
for programmatic access and human-readable formatting for interactive use.
-}

module HsJupyter.CLI.Output
    ( -- * Output Formatting
      OutputFormat(..)
    , formatOutput
    , formatCommandOutput
    
      -- * JSON Output Generation
    , toJSONOutput
    , successOutput
    , errorOutput
    , diagnosticOutput
    
      -- * Human-Readable Formatting  
    , formatInstallationResult
    , formatDiagnosticResult
    , formatListResult
    , formatVersionResult
    
      -- * Output Utilities
    , printOutput
    , writeOutputFile
    ) where

import Data.Aeson (Value(..), ToJSON(..), (.=), object)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO (stderr, hPutStrLn)
import Control.Monad.IO.Class (MonadIO, liftIO)

import HsJupyter.CLI.Types
    ( CLIDiagnostic(..)
    , DiagnosticResult(..)
    , Issue(..)
    , KernelInstallation(..)
    , iDescription
    , iDetails
    )

-- | Output format specification
data OutputFormat
    = HumanReadable    -- ^ Human-friendly formatted output
    | JSON            -- ^ Structured JSON output  
    | Quiet           -- ^ Minimal output (success/failure only)
    deriving (Show, Eq)

-- ===========================================================================
-- T035: JSON Output Generation for All Commands
-- ===========================================================================

-- | Format command output based on specified format
formatOutput :: OutputFormat -> Either CLIDiagnostic Value -> IO ()
formatOutput format result = 
    case format of
        JSON -> printJSON result
        HumanReadable -> printHuman result
        Quiet -> printQuiet result

-- | Generate JSON output for any command result  
toJSONOutput :: Either CLIDiagnostic Value -> Value
toJSONOutput (Right result) = result
toJSONOutput (Left diagnostic) = errorOutput diagnostic

-- | Create success JSON output with structured data
successOutput :: Text -> Value -> Value
successOutput message payload = object
    [ "status" .= ("success" :: Text)
    , "message" .= message
    , "result" .= payload
    ]

-- | Create error JSON output from diagnostic
errorOutput :: CLIDiagnostic -> Value
errorOutput diagnostic = object
    [ "status" .= ("error" :: Text)
    , "error" .= object
        [ "type" .= diagnosticType diagnostic
        , "message" .= diagnosticMessage diagnostic
        , "details" .= diagnosticDetails diagnostic
        ]
    ]

-- | Create diagnostic-specific JSON output
diagnosticOutput :: DiagnosticResult -> Value
diagnosticOutput result = object
    [ "status" .= ("success" :: Text)
    , "message" .= ("System diagnostic completed" :: Text)
    , "result" .= result
    ]

-- ===========================================================================
-- Command-Specific Output Formatting
-- ===========================================================================

-- | Format installation command output
formatCommandOutput :: OutputFormat -> Text -> Either CLIDiagnostic KernelInstallation -> IO ()
formatCommandOutput format operation result =
    case format of
        JSON -> printJSON $ case result of
            Right installation -> Right $ successOutput operation (toJSON installation)
            Left diagnostic -> Left diagnostic
        HumanReadable -> printHuman $ case result of
            Right installation -> Right $ formatInstallationResult installation
            Left diagnostic -> Left diagnostic
        Quiet -> printQuiet $ case result of
            Right _ -> Right $ String "OK"
            Left diagnostic -> Left diagnostic

-- | Format installation result for human reading
formatInstallationResult :: KernelInstallation -> Value
formatInstallationResult installation = object
    [ "type" .= ("installation_result" :: Text)
    , "kernelspec_path" .= kiKernelspecPath installation
    , "display_name" .= kiDisplayName installation
    , "version" .= kiVersion installation
    , "ghc_path" .= kiGHCPath installation
    , "status" .= show (kiStatus installation)
    ]

-- | Format diagnostic result for human reading
formatDiagnosticResult :: DiagnosticResult -> Value
formatDiagnosticResult result = object
    [ "type" .= ("diagnostic_result" :: Text)
    , "overall_status" .= show (drOverallStatus result)
    , "issues_found" .= length (drIssuesFound result)
    , "recommendations" .= length (drRecommendations result)
    , "issues" .= drIssuesFound result
    , "recommendations" .= drRecommendations result
    ]

-- | Format kernel list result
formatListResult :: [KernelInstallation] -> Value
formatListResult installations = object
    [ "type" .= ("list_result" :: Text)
    , "total_installations" .= length installations
    , "installations" .= installations
    ]

-- | Format version information result
formatVersionResult :: Text -> Text -> Value
formatVersionResult version buildInfo = object
    [ "type" .= ("version_result" :: Text)
    , "version" .= version
    , "build_info" .= buildInfo
    , "compatible" .= True
    ]

-- ===========================================================================
-- Output Utilities
-- ===========================================================================

-- | Print output to stdout based on format
printOutput :: MonadIO m => OutputFormat -> Either CLIDiagnostic Value -> m ()
printOutput format result = liftIO $ formatOutput format result

-- | Write output to file with specified format
writeOutputFile :: MonadIO m => FilePath -> OutputFormat -> Either CLIDiagnostic Value -> m ()
writeOutputFile filePath format result = liftIO $ do
    let content = case format of
            JSON -> L8.unpack $ Pretty.encodePretty $ toJSONOutput result
            HumanReadable -> formatHumanText result
            Quiet -> formatQuietText result
    writeFile filePath content

-- ===========================================================================
-- Internal Output Implementation
-- ===========================================================================

-- | Print JSON output to stdout
printJSON :: Either CLIDiagnostic Value -> IO ()
printJSON result = do
    L8.putStrLn $ Pretty.encodePretty $ toJSONOutput result

-- | Print human-readable output to stdout/stderr
printHuman :: Either CLIDiagnostic Value -> IO ()
printHuman result = do
    case result of
        Right value -> TIO.putStrLn $ formatValueHuman value
        Left diagnostic -> do
            hPutStrLn stderr $ "Error: " ++ T.unpack (diagnosticMessage diagnostic)
            case diagnosticDetails diagnostic of
                Just details -> hPutStrLn stderr $ "Details: " ++ T.unpack details
                Nothing -> return ()

-- | Print quiet output (minimal)
printQuiet :: Either CLIDiagnostic Value -> IO ()
printQuiet result =
    case result of
        Right _ -> return ()  -- Success: no output in quiet mode
        Left diagnostic -> hPutStrLn stderr $ T.unpack $ diagnosticMessage diagnostic

-- | Format value for human reading
formatValueHuman :: Value -> Text
formatValueHuman (Object obj) = 
    case KM.lookup (Key.fromText "type") obj of
        Just (String "installation_result") -> formatInstallationHuman obj
        Just (String "diagnostic_result") -> formatDiagnosticHuman obj
        Just (String "list_result") -> formatListHuman obj
        Just (String "version_result") -> formatVersionHuman obj
        _ -> "Operation completed successfully"
formatValueHuman _ = "Operation completed successfully"

-- | Format installation result for human reading
formatInstallationHuman :: A.Object -> Text
formatInstallationHuman obj = T.unlines
    [ "✅ Kernel installation completed successfully!"
    , "📁 Kernelspec path: " <> extractText (Key.fromText "kernelspec_path") obj
    , "🏷️  Display name: " <> extractText (Key.fromText "display_name") obj
    , "📦 Version: " <> extractText (Key.fromText "version") obj
    , "🔧 GHC path: " <> extractText (Key.fromText "ghc_path") obj
    ]

-- | Format diagnostic result for human reading
formatDiagnosticHuman :: A.Object -> Text
formatDiagnosticHuman obj = T.unlines
    [ "🔍 System diagnostic completed"
    , "📊 Overall status: " <> extractText (Key.fromText "overall_status") obj
    , "⚠️  Issues found: " <> T.pack (show $ extractNumber (Key.fromText "issues_found") obj)
    , "💡 Recommendations: " <> T.pack (show $ extractNumber (Key.fromText "recommendations") obj)
    ]

-- | Format list result for human reading
formatListHuman :: A.Object -> Text
formatListHuman obj = 
    "📋 Found " <> T.pack (show $ extractNumber (Key.fromText "total_installations") obj) <> " HsJupyter kernel installations"

-- | Format version result for human reading
formatVersionHuman :: A.Object -> Text
formatVersionHuman obj = T.unlines
    [ "📦 HsJupyter Kernel Version: " <> extractText (Key.fromText "version") obj
    , "🏗️  Build info: " <> extractText (Key.fromText "build_info") obj
    , "✅ System compatibility: OK"
    ]

-- | Format result as human text
formatHumanText :: Either CLIDiagnostic Value -> String
formatHumanText result = 
    T.unpack $ case result of
        Right value -> formatValueHuman value
        Left diagnostic -> "Error: " <> diagnosticMessage diagnostic

-- | Format result as quiet text
formatQuietText :: Either CLIDiagnostic Value -> String
formatQuietText result = 
    case result of
        Right _ -> ""
        Left diagnostic -> T.unpack $ diagnosticMessage diagnostic

-- ===========================================================================
-- Diagnostic Helper Functions
-- ===========================================================================

-- | Extract diagnostic type for JSON output
diagnosticType :: CLIDiagnostic -> Text
diagnosticType (InstallationError _) = "InstallationError"
diagnosticType (ValidationError _) = "ValidationError"
diagnosticType (ConfigurationError _) = "ConfigurationError"
diagnosticType (SystemIntegrationError _) = "SystemIntegrationError"

-- | Extract diagnostic message
diagnosticMessage :: CLIDiagnostic -> Text
diagnosticMessage (InstallationError issue) = iDescription issue
diagnosticMessage (ValidationError msg) = msg
diagnosticMessage (ConfigurationError msg) = msg
diagnosticMessage (SystemIntegrationError msg) = msg

-- | Extract diagnostic details if available
diagnosticDetails :: CLIDiagnostic -> Maybe Text
diagnosticDetails (InstallationError issue) = iDetails issue
diagnosticDetails _ = Nothing

-- | Extract text field from JSON object
extractText :: Key.Key -> A.Object -> Text
extractText key obj = 
    case KM.lookup key obj of
        Just (String text) -> text
        _ -> ""

-- | Extract number field from JSON object
extractNumber :: Key.Key -> A.Object -> Int
extractNumber key obj = 
    case KM.lookup key obj of
        Just (Number n) -> truncate n
        _ -> 0
