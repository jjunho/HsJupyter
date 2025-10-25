{-# LANGUAGE OverloadedStrings #-}

module DiagnosticsSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
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

  describe "enhanced error translation scenarios" $ do
    it "handles GHC parse errors with detailed span information" $ do
      let parseError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Parse error: unexpected token"
            , rdDetail = Just "parse error on input 'where'\nPerhaps you meant to use BlockArguments?"
            , rdSpan = Just $ DiagnosticSpan (Just "Cell.hs") (Just 12) (Just 15) (Just 12) (Just 20)
            , rdSuggestions = ["Enable BlockArguments extension", "Check syntax around 'where' clause"]
            , rdSource = Just "ghc-parser"
            }
      
      rdSeverity parseError `shouldBe` SeverityError
      rdSummary parseError `shouldBe` "Parse error: unexpected token"
      rdSpan parseError `shouldSatisfy` (\span -> case span of
        Just s -> spanStartLine s == Just 12 && spanStartCol s == Just 15
        Nothing -> False)

    it "handles type checking errors with context" $ do
      let typeError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Couldn't match expected type"
            , rdDetail = Just "Couldn't match expected type 'Int' with actual type '[Char]'\nIn the expression: \"hello\"\nIn an equation for 'x'"
            , rdSpan = Just $ DiagnosticSpan (Just "Cell.hs") (Just 3) (Just 5) (Just 3) (Just 12)
            , rdSuggestions = ["Use 'read \"hello\"' if you meant to parse", "Change type signature", "Use a string type instead"]
            , rdSource = Just "ghc-typechecker"
            }
      
      rdDetail typeError `shouldSatisfy` (\detail -> case detail of
        Just d -> T.isInfixOf "Couldn't match expected type" d
        Nothing -> False)

    it "handles module import errors" $ do
      let importError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Could not find module"
            , rdDetail = Just "Could not find module 'Data.NonExistent'\nUse -v to see a list of the files searched for."
            , rdSpan = Just $ DiagnosticSpan (Just "Cell.hs") (Just 1) (Just 1) (Just 1) (Just 25)
            , rdSuggestions = ["Check module name spelling", "Install required package", "Add module to dependencies"]
            , rdSource = Just "ghc-modules"
            }
      
      rdSummary importError `shouldBe` "Could not find module"
      length (rdSuggestions importError) `shouldBe` 3

    it "handles runtime exceptions with stack traces" $ do
      let runtimeException = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Exception: Prelude.head: empty list"
            , rdDetail = Just "*** Exception: Prelude.head: empty list\nCallStack (from HasCallStack):\n  error, called at libraries/base/GHC/List.hs:1646:3"
            , rdSpan = Nothing -- Runtime errors typically don't have source spans
            , rdSuggestions = ["Use 'listToMaybe' instead of 'head'", "Check list is non-empty before calling head", "Handle the exception"]
            , rdSource = Just "ghc-runtime"
            }
      
      rdSeverity runtimeException `shouldBe` SeverityError
      rdSpan runtimeException `shouldBe` Nothing

    it "handles timeout errors" $ do
      let timeoutError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Evaluation timeout"
            , rdDetail = Just "Code execution exceeded the 10 second timeout limit"
            , rdSpan = Nothing
            , rdSuggestions = ["Optimize algorithm for better performance", "Increase timeout limit", "Check for infinite loops"]
            , rdSource = Just "runtime-guard"
            }
      
      rdSummary timeoutError `shouldBe` "Evaluation timeout"
      rdSource timeoutError `shouldBe` Just "runtime-guard"

    it "handles resource limit violations" $ do
      let memoryError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Memory limit exceeded"
            , rdDetail = Just "Process exceeded memory limit of 512MB (used: 756MB)"
            , rdSpan = Nothing
            , rdSuggestions = ["Reduce memory usage", "Use streaming operations", "Increase memory limit"]
            , rdSource = Just "resource-guard"
            }
      
      rdSummary memoryError `shouldBe` "Memory limit exceeded"
      rdDetail memoryError `shouldSatisfy` (\detail -> case detail of
        Just d -> T.isInfixOf "512MB" d
        Nothing -> False)

  describe "diagnostic severity classification" $ do
    it "correctly classifies compilation errors as Error severity" $ do
      let compileErrors = 
            [ mkError "Parse error"
            , mkError "Type mismatch"
            , mkError "Variable not in scope"
            ]
      
      all ((== SeverityError) . rdSeverity) compileErrors `shouldBe` True

    it "correctly classifies warnings as Warning severity" $ do
      let warnings = 
            [ mkWarning "Unused variable"
            , mkWarning "Redundant import"
            , mkWarning "Missing type signature"
            ]
      
      all ((== SeverityWarning) . rdSeverity) warnings `shouldBe` True

    it "correctly classifies informational messages" $ do
      let infoMessages = 
            [ mkInfo "Compilation started"
            , mkInfo "Loading package"
            , mkInfo "Compilation completed"
            ]
      
      all ((== SeverityInfo) . rdSeverity) infoMessages `shouldBe` True

  describe "suggestion system" $ do
    it "provides helpful suggestions for common errors" $ do
      let undefinedVarError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Variable not in scope: xyz"
            , rdDetail = Nothing
            , rdSpan = Just $ DiagnosticSpan (Just "Cell.hs") (Just 5) (Just 10) (Just 5) (Just 13)
            , rdSuggestions = ["Define variable 'xyz'", "Import module containing 'xyz'", "Check spelling of variable name"]
            , rdSource = Just "ghc"
            }
      
      length (rdSuggestions undefinedVarError) `shouldBe` 3
      "Define variable" `shouldSatisfy` (\text -> any (T.isInfixOf text) (rdSuggestions undefinedVarError))

    it "provides context-specific suggestions" $ do
      let invalidSyntaxError = RuntimeDiagnostic
            { rdSeverity = SeverityError
            , rdSummary = "Invalid syntax"
            , rdDetail = Just "Expected '=' but found '=='"
            , rdSpan = Just $ DiagnosticSpan (Just "Cell.hs") (Just 2) (Just 8) (Just 2) (Just 10)
            , rdSuggestions = ["Use '=' for assignment", "Use '==' for comparison in conditionals"]
            , rdSource = Just "ghc-parser"
            }
      
      rdSuggestions invalidSyntaxError `shouldSatisfy` (elem "Use '=' for assignment")

    it "can have empty suggestions for some diagnostics" $ do
      let internalError = mkError "Internal compiler error"
      
      rdSuggestions internalError `shouldBe` []

  describe "JSON serialization edge cases" $ do
    it "handles diagnostics with unicode characters" $ do
      let unicodeError = mkError "Parse error: unexpected '→' symbol"
          encoded = encode unicodeError
          decoded = decode encoded
      
      decoded `shouldBe` Just unicodeError

    it "handles very long error messages" $ do
      let longMessage = T.replicate 1000 "x"
          longError = mkError longMessage
          encoded = encode longError
          decoded = decode encoded
      
      decoded `shouldBe` Just longError
      rdSummary <$> decoded `shouldBe` Just longMessage

    it "handles empty suggestion lists correctly" $ do
      let diagnostic = mkWarning "Simple warning"
          encoded = encode diagnostic
          decoded = decode encoded
      
      rdSuggestions <$> decoded `shouldBe` Just []