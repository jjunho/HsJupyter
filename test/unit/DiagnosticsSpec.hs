{-# LANGUAGE OverloadedStrings #-}

module DiagnosticsSpec (spec) where

import Data.Aeson (decode, encode)
import Test.Hspec

import HsJupyter.Runtime.Diagnostics
  ( DiagnosticSeverity(..)
  , DiagnosticSpan(..)
  , RuntimeDiagnostic(..)
  , mkDiagnostic
  , mkError
  , mkWarning
  , mkInfo
  )

spec :: Spec
spec = describe "Runtime Diagnostics" $ do
  describe "DiagnosticSeverity" $ do
    it "has correct ordering (Info < Warning < Error)" $ do
      SeverityInfo < SeverityWarning `shouldBe` True
      SeverityWarning < SeverityError `shouldBe` True
      SeverityInfo < SeverityError `shouldBe` True

    it "serializes to JSON correctly" $ do
      let infoJson = encode SeverityInfo
          warningJson = encode SeverityWarning
          errorJson = encode SeverityError
      
      decode infoJson `shouldBe` Just SeverityInfo
      decode warningJson `shouldBe` Just SeverityWarning
      decode errorJson `shouldBe` Just SeverityError

    it "deserializes from JSON correctly" $ do
      decode "\"info\"" `shouldBe` Just SeverityInfo
      decode "\"warning\"" `shouldBe` Just SeverityWarning
      decode "\"error\"" `shouldBe` Just SeverityError
      decode "\"unknown\"" `shouldBe` Just SeverityInfo  -- defaults to info

  describe "DiagnosticSpan" $ do
    it "can represent file locations with line/column info" $ do
      let sourceSpan = DiagnosticSpan
            { spanFile = Just "test.hs"
            , spanStartLine = Just 10
            , spanStartCol = Just 5
            , spanEndLine = Just 10
            , spanEndCol = Just 15
            }
      
      spanFile sourceSpan `shouldBe` Just "test.hs"
      spanStartLine sourceSpan `shouldBe` Just 10
      spanStartCol sourceSpan `shouldBe` Just 5

    it "can represent minimal spans with just line numbers" $ do
      let sourceSpan = DiagnosticSpan
            { spanFile = Nothing
            , spanStartLine = Just 5
            , spanStartCol = Nothing
            , spanEndLine = Nothing
            , spanEndCol = Nothing
            }
      
      spanStartLine sourceSpan `shouldBe` Just 5
      spanFile sourceSpan `shouldBe` Nothing

    it "serializes and deserializes correctly" $ do
      let sourceSpan = DiagnosticSpan (Just "test.hs") (Just 1) (Just 1) (Just 2) (Just 10)
          encoded = encode sourceSpan
          decoded = decode encoded
      
      decoded `shouldBe` Just sourceSpan

  describe "RuntimeDiagnostic" $ do
    it "can be created with mkDiagnostic helper" $ do
      let diag = mkDiagnostic SeverityError "Test error message"
      
      rdSeverity diag `shouldBe` SeverityError
      rdSummary diag `shouldBe` "Test error message"
      rdDetail diag `shouldBe` Nothing
      rdSpan diag `shouldBe` Nothing
      rdSuggestions diag `shouldBe` []
      rdSource diag `shouldBe` Nothing

    it "can be created with convenience helpers" $ do
      let errorDiag = mkError "Error message"
          warningDiag = mkWarning "Warning message" 
          infoDiag = mkInfo "Info message"
      
      rdSeverity errorDiag `shouldBe` SeverityError
      rdSeverity warningDiag `shouldBe` SeverityWarning
      rdSeverity infoDiag `shouldBe` SeverityInfo
      
      rdSummary errorDiag `shouldBe` "Error message"
      rdSummary warningDiag `shouldBe` "Warning message"
      rdSummary infoDiag `shouldBe` "Info message"

    it "can include detailed information" $ do
      let sourceSpan = DiagnosticSpan (Just "test.hs") (Just 5) (Just 1) (Just 5) (Just 10)
          diag = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Parse error"
            , rdDetail = Just "Expected '}' but found end of input"
            , rdSpan = Just sourceSpan
            , rdSuggestions = ["Add closing brace", "Check syntax"]
            , rdSource = Just "parser"
            }
      
      rdDetail diag `shouldBe` Just "Expected '}' but found end of input"
      length (rdSuggestions diag) `shouldBe` 2
      rdSource diag `shouldBe` Just "parser"

    it "serializes to JSON with all fields" $ do
      let sourceSpan = DiagnosticSpan (Just "test.hs") (Just 1) (Just 1) (Just 1) (Just 5)
          diag = RuntimeDiagnostic
            { rdSeverity = SeverityWarning
            , rdSummary = "Unused variable"
            , rdDetail = Just "Variable 'x' is defined but never used"
            , rdSpan = Just sourceSpan
            , rdSuggestions = ["Remove the variable", "Use the variable"]
            , rdSource = Just "lint"
            }
          encoded = encode diag
          decoded = decode encoded
      
      decoded `shouldBe` Just diag

    it "handles missing optional fields gracefully" $ do
      let diag = mkError "Simple error"
          encoded = encode diag
          decoded = decode encoded
      
      decoded `shouldBe` Just diag

  describe "error translation scenarios" $ do
    it "can represent compilation errors" $ do
      let compileError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Type mismatch"
            , rdDetail = Just "Expected: Int\nActual: String"
            , rdSpan = Just $ DiagnosticSpan (Just "Main.hs") (Just 15) (Just 8) (Just 15) (Just 13)
            , rdSuggestions = ["Convert String to Int using 'read'", "Change type annotation"]
            , rdSource = Just "ghc"
            }
      
      rdSeverity compileError `shouldBe` SeverityError
      rdSummary compileError `shouldBe` "Type mismatch"
      rdSource compileError `shouldBe` Just "ghc"

    it "can represent runtime errors" $ do
      let runtimeError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Division by zero"
            , rdDetail = Just "Arithmetic exception: divide by zero"
            , rdSpan = Nothing  -- Runtime errors might not have source locations
            , rdSuggestions = ["Check divisor before division", "Handle exception"]
            , rdSource = Just "runtime"
            }
      
      rdSeverity runtimeError `shouldBe` SeverityError
      rdSummary runtimeError `shouldBe` "Division by zero"
      rdSpan runtimeError `shouldBe` Nothing

    it "can represent linting warnings" $ do
      let lintWarning = RuntimeDiagnostic
            { rdSeverity = SeverityWarning
            , rdSummary = "Redundant import"
            , rdDetail = Just "Import of 'Data.List' is redundant"
            , rdSpan = Just $ DiagnosticSpan (Just "Main.hs") (Just 3) (Just 1) (Just 3) (Just 20)
            , rdSuggestions = ["Remove redundant import"]
            , rdSource = Just "hlint"
            }
      
      rdSeverity lintWarning `shouldBe` SeverityWarning
      rdSummary lintWarning `shouldBe` "Redundant import"

  describe "diagnostic aggregation" $ do
    it "can collect multiple diagnostics" $ do
      let diagnostics = 
            [ mkError "Parse error on line 5"
            , mkWarning "Unused variable on line 10"
            , mkInfo "Compilation completed"
            ]
      
      length diagnostics `shouldBe` 3
      
      let errors = filter ((== SeverityError) . rdSeverity) diagnostics
          warnings = filter ((== SeverityWarning) . rdSeverity) diagnostics
          infos = filter ((== SeverityInfo) . rdSeverity) diagnostics
      
      length errors `shouldBe` 1
      length warnings `shouldBe` 1
      length infos `shouldBe` 1

    it "can prioritize diagnostics by severity" $ do
      let diagnostics = 
            [ mkInfo "Info message"
            , mkError "Error message"  
            , mkWarning "Warning message"
            ]
          sortedBySeverity = [ d | d <- diagnostics, rdSeverity d == SeverityError ] ++
                            [ d | d <- diagnostics, rdSeverity d == SeverityWarning ] ++
                            [ d | d <- diagnostics, rdSeverity d == SeverityInfo ]
      
      map rdSeverity sortedBySeverity `shouldBe` [SeverityError, SeverityWarning, SeverityInfo]