-- | Unit tests for GHC diagnostics and error handling
-- Tests error mapping and diagnostic reporting
module GHCDiagnosticsSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Language.Haskell.Interpreter (InterpreterError(..), GhcError(..))

import HsJupyter.Runtime.GHCDiagnostics
import HsJupyter.Runtime.Diagnostics (DiagnosticSeverity(..), RuntimeDiagnostic(..))

spec :: Spec
spec = describe "GHCDiagnostics" $ do
  describe "source location extraction" $ do
    it "extracts line and column from GHC error messages" $ do
      let location = extractSourceLocation "<interactive>:5:12: error message"
      sourceLine location `shouldBe` 5
      sourceColumn location `shouldBe` 12

    it "handles missing location information gracefully" $ do
      let location = extractSourceLocation "error without location"
      sourceLine location `shouldBe` 1
      sourceColumn location `shouldBe` 1

    it "parses file-based error locations" $ do
      let location = extractSourceLocation "Main.hs:10:5: parse error"
      sourceLine location `shouldBe` 10
      sourceColumn location `shouldBe` 5

  describe "syntax error detection" $ do
    it "detects unterminated string errors" $ do
      let errorType = detectSyntaxErrorType "lexical error in string"
      errorType `shouldBe` Just UnterminatedString

    it "detects indentation errors" $ do
      let errorType = detectSyntaxErrorType "indentation error"
      errorType `shouldBe` Just IndentationError

    it "detects unbalanced delimiters" $ do
      let errorType = detectSyntaxErrorType "missing closing bracket"
      errorType `shouldBe` Just UnbalancedDelimiters

    it "returns Nothing for non-syntax errors" $ do
      let errorType = detectSyntaxErrorType "type error message"
      errorType `shouldBe` Nothing

  describe "undefined variable detection" $ do
    it "extracts variable names from scope errors" $ do
      let varName = extractUndefinedVariable "Variable not in scope: myVar"
      varName `shouldBe` Just "myVar"

    it "handles quoted variable names" $ do
      let varName = extractUndefinedVariable "Not in scope: 'someFunc'"
      varName `shouldBe` Just "someFunc"

    it "returns Nothing for non-variable errors" $ do
      let varName = extractUndefinedVariable "parse error"
      varName `shouldBe` Nothing

  describe "error classification" $ do
    it "classifies type errors correctly" $ do
      let ghcError = GhcError "Couldn't match expected type 'Int' with actual type 'String'"
      let result = interpretError (WontCompile [ghcError])
      case result of
        CompilationError msg _ suggestions -> do
          msg `shouldSatisfy` T.isInfixOf "Couldn't match expected type"
          suggestions `shouldNotBe` []
        _ -> expectationFailure "Expected CompilationError"

    it "classifies syntax errors correctly" $ do
      let ghcError = GhcError "parse error on input '='"
      let result = interpretError (WontCompile [ghcError])
      case result of
        CompilationError msg _ suggestions -> do
          msg `shouldSatisfy` T.isInfixOf "parse error"
          suggestions `shouldNotBe` []
        _ -> expectationFailure "Expected CompilationError"

    it "classifies name errors correctly" $ do
      let ghcError = GhcError "Variable not in scope: undefined_var"
      let result = interpretError (WontCompile [ghcError])
      case result of
        CompilationError msg _ suggestions -> do
          msg `shouldSatisfy` T.isInfixOf "not in scope"
          suggestions `shouldSatisfy` any (T.isInfixOf "undefined_var")
        _ -> expectationFailure "Expected CompilationError"

  describe "suggestion generation" $ do
    it "provides type-specific suggestions for Char/String confusion" $ do
      let suggestions = generateTypeSuggestions "Expected Char but got String"
      suggestions `shouldSatisfy` any (T.isInfixOf "single quotes")
      suggestions `shouldSatisfy` any (T.isInfixOf "double quotes")

    it "provides variable-specific suggestions for undefined variables" $ do
      let suggestions = generateNameSuggestions "Variable not in scope: myFunction"
      suggestions `shouldSatisfy` any (T.isInfixOf "myFunction")
      suggestions `shouldSatisfy` any (T.isInfixOf "Check spelling")

    it "provides syntax-specific suggestions for different error types" $ do
      let suggestions = generateEnhancedSyntaxSuggestions "unterminated string literal"
      suggestions `shouldSatisfy` any (T.isInfixOf "Close the string")

  describe "common misspelling detection" $ do
    it "suggests corrections for common typos" $ do
      let suggestions = generateNameSuggestions "Variable not in scope: lenght"
      suggestions `shouldSatisfy` any (T.isInfixOf "length")

    it "suggests corrections for filter misspelling" $ do
      let suggestions = generateNameSuggestions "Variable not in scope: fiter"
      suggestions `shouldSatisfy` any (T.isInfixOf "filter")

  describe "diagnostic enrichment" $ do
    it "enriches diagnostics with suggestions" $ do
      let ghcError = CompilationError "test error" (SourceLocation 1 1 Nothing) ["suggestion 1", "suggestion 2"]
      let diagnostic = ghcErrorToDiagnostic ghcError
      let enriched = enrichDiagnostic ghcError diagnostic
      rdSuggestions enriched `shouldContain` ["suggestion 1", "suggestion 2"]
      length (rdSuggestions enriched) `shouldSatisfy` (>= 2)

  describe "integration with RuntimeDiagnostic" $ do
    it "converts GHCError to RuntimeDiagnostic correctly" $ do
      let ghcError = CompilationError "test compilation error" (SourceLocation 5 10 Nothing) []
      let diagnostic = ghcErrorToDiagnostic ghcError
      rdSeverity diagnostic `shouldBe` SeverityError
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "test compilation error"
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "5:10"

    it "handles timeout errors appropriately" $ do
      let ghcError = TimeoutError 30
      let diagnostic = ghcErrorToDiagnostic ghcError
      rdSeverity diagnostic `shouldBe` SeverityError
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "30 seconds"

    it "handles import errors with module information" $ do
      let ghcError = ImportError "NonExistent.Module" "Could not find module"
      let diagnostic = ghcErrorToDiagnostic ghcError
      rdSeverity diagnostic `shouldBe` SeverityError
      rdSummary diagnostic `shouldSatisfy` T.isInfixOf "NonExistent.Module"