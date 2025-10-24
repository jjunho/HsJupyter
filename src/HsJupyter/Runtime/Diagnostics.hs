{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Runtime.Diagnostics
  ( DiagnosticSeverity(..)
  , DiagnosticSpan(..)
  , RuntimeDiagnostic(..)
  , mkDiagnostic
  , mkError
  , mkWarning
  , mkInfo
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , (.!=)
  , object
  , withObject
  , withText
  )
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Severity levels for runtime diagnostics.
data DiagnosticSeverity
  = SeverityInfo
  | SeverityWarning
  | SeverityError
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON DiagnosticSeverity where
  toJSON SeverityInfo    = String "info"
  toJSON SeverityWarning = String "warning"
  toJSON SeverityError   = String "error"

instance FromJSON DiagnosticSeverity where
  parseJSON = withText "DiagnosticSeverity" $ \t ->
    pure $ case T.toLower t of
      "warning" -> SeverityWarning
      "error"   -> SeverityError
      _         -> SeverityInfo

-- | Optional span information for diagnostics.
data DiagnosticSpan = DiagnosticSpan
  { spanFile      :: Maybe FilePath
  , spanStartLine :: Maybe Int
  , spanStartCol  :: Maybe Int
  , spanEndLine   :: Maybe Int
  , spanEndCol    :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Structured diagnostic returned with execution outcomes.
data RuntimeDiagnostic = RuntimeDiagnostic
  { rdSeverity     :: DiagnosticSeverity
  , rdSummary      :: Text
  , rdDetail       :: Maybe Text
  , rdSpan         :: Maybe DiagnosticSpan
  , rdSuggestions  :: [Text]
  , rdSource       :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RuntimeDiagnostic where
  toJSON diag = object
    [ "severity"    .= rdSeverity diag
    , "summary"     .= rdSummary diag
    , "detail"      .= rdDetail diag
    , "span"        .= rdSpan diag
    , "suggestions" .= rdSuggestions diag
    , "source"      .= rdSource diag
    ]

instance FromJSON RuntimeDiagnostic where
  parseJSON = withObject "RuntimeDiagnostic" $ \obj ->
    RuntimeDiagnostic
      <$> obj .:  "severity"
      <*> obj .:  "summary"
      <*> obj .:? "detail"
      <*> obj .:? "span"
      <*> obj .:? "suggestions" .!= []
      <*> obj .:? "source"

-- | Construct a diagnostic with the supplied severity.
mkDiagnostic
  :: DiagnosticSeverity
  -> Text
  -> RuntimeDiagnostic
mkDiagnostic sev summary =
  RuntimeDiagnostic sev summary Nothing Nothing [] Nothing

mkError :: Text -> RuntimeDiagnostic
mkError = mkDiagnostic SeverityError

mkWarning :: Text -> RuntimeDiagnostic
mkWarning = mkDiagnostic SeverityWarning

mkInfo :: Text -> RuntimeDiagnostic
mkInfo = mkDiagnostic SeverityInfo
