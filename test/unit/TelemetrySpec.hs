{-# LANGUAGE OverloadedStrings #-}

module TelemetrySpec (spec) where

import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import Test.Hspec

import HsJupyter.Runtime.Diagnostics
import HsJupyter.Runtime.SessionState
import HsJupyter.Runtime.Telemetry

spec :: Spec
spec = describe "Runtime telemetry" $ do
  it "produces execution metrics" $ do
    let outcome = ExecutionOutcome
          { outcomeStatus = ExecutionOk
          , outcomeStreams = []
          , outcomePayload = []
          , outcomeDiagnostics = []
          , outcomeExecutionCount = 1
          , outcomeDuration = zeroDuration
          }
        metric = executionMetric outcome (secondsToNominalDiffTime 2)
    metricName metric `shouldBe` "runtime_execution_duration_seconds"
    metricLabels metric `shouldBe` [("status", "ok")]

  it "produces diagnostic metrics" $ do
    let diag = mkWarning "Unused variable"
        metric = diagnosticMetric diag
    metricName metric `shouldBe` "runtime_diagnostic_total"
    metricLabels metric `shouldContain` [("severity", "warning")]

  it "produces resource limit metrics" $ do
    let outcome = ExecutionOutcome
          { outcomeStatus = ExecutionResourceLimit
          , outcomeStreams = []
          , outcomePayload = []
          , outcomeDiagnostics = []
          , outcomeExecutionCount = 0
          , outcomeDuration = secondsToNominalDiffTime 0
          }
        metric = resourceLimitMetric outcome
    metricLabels metric `shouldBe` [("status", "resource-limit")]

zeroDuration :: NominalDiffTime
zeroDuration = secondsToNominalDiffTime 0
