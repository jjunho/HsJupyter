{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Runtime.Telemetry
  ( RuntimeMetric(..)
  , emitMetric
  , emitMetrics
  , executionMetric
  , cancellationMetric
  , resourceLimitMetric
  , diagnosticMetric
  ) where

import Data.Aeson (Value(..), toJSON)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)

import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic(..), DiagnosticSeverity(..))
import HsJupyter.Runtime.SessionState (ExecutionOutcome(..), ExecutionStatus(..))

-- | Lightweight metric representation ready for export to logging/metrics sinks.
data RuntimeMetric = RuntimeMetric
  { metricName   :: Text
  , metricValue  :: Value
  , metricLabels :: [(Text, Text)]
  }
  deriving (Eq, Show)

emitMetric :: (RuntimeMetric -> IO ()) -> RuntimeMetric -> IO ()
emitMetric sink metric = sink metric

emitMetrics :: (RuntimeMetric -> IO ()) -> [RuntimeMetric] -> IO ()
emitMetrics sink = mapM_ sink

executionMetric :: ExecutionOutcome -> NominalDiffTime -> RuntimeMetric
executionMetric outcome duration =
  RuntimeMetric
    "runtime_execution_duration_seconds"
    (toJSON (realToFrac duration :: Double))
    [ ("status", statusLabel (outcomeStatus outcome))
    ]

cancellationMetric :: ExecutionOutcome -> RuntimeMetric
cancellationMetric outcome =
  RuntimeMetric
    "runtime_cancellation_total"
    (toJSON (1 :: Int))
    [ ("status", statusLabel (outcomeStatus outcome))
    ]

resourceLimitMetric :: ExecutionOutcome -> RuntimeMetric
resourceLimitMetric outcome =
  RuntimeMetric
    "runtime_resource_limit_total"
    (toJSON (1 :: Int))
    [ ("status", statusLabel (outcomeStatus outcome))
    ]

diagnosticMetric :: RuntimeDiagnostic -> RuntimeMetric
diagnosticMetric diag =
  RuntimeMetric
    "runtime_diagnostic_total"
    (toJSON (1 :: Int))
    [ ("severity", severityLabel (rdSeverity diag))
    , ("summary", rdSummary diag)
    ]

statusLabel :: ExecutionStatus -> Text
statusLabel ExecutionOk             = "ok"
statusLabel ExecutionError          = "error"
statusLabel ExecutionAbort          = "abort"
statusLabel ExecutionResourceLimit  = "resource-limit"

severityLabel :: DiagnosticSeverity -> Text
severityLabel SeverityInfo    = "info"
severityLabel SeverityWarning = "warning"
severityLabel SeverityError   = "error"
