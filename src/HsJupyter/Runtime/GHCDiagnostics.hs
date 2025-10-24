{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Runtime.GHCDiagnostics
  ( -- * Error types
    GHCError(..)
  , GHCErrorType(..)
  , SourceLocation(..)
    
    -- * Error conversion
  , interpretError
  , ghcErrorToDiagnostic
  , enrichDiagnostic
    
    -- * Suggestion system
  , generateSuggestions
  , commonErrorSuggestions
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.Interpreter (InterpreterError(..), GhcError(..))

import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic(..), DiagnosticSeverity(..), mkDiagnostic)

-- | GHC-specific error types with detailed information
data GHCError
  = CompilationError Text SourceLocation [Text]  -- message, location, suggestions
  | RuntimeError Text                            -- runtime exception message
  | TimeoutError Int                             -- timeout in seconds
  | ImportError String Text                      -- module name, error message
  | SecurityError Text                           -- security policy violation
  deriving (Show, Eq)

-- | Classification of GHC error types
data GHCErrorType
  = SyntaxError
  | TypeError  
  | NameError
  | ImportError'
  | RuntimeError'
  | TimeoutError'
  | SecurityError'
  deriving (Show, Eq)

-- | Source location information from GHC errors
data SourceLocation = SourceLocation
  { sourceLine :: Int
  , sourceColumn :: Int
  , sourceFile :: Maybe FilePath
  } deriving (Show, Eq)

-- | Convert hint InterpreterError to GHCError
interpretError :: InterpreterError -> GHCError
interpretError err = case err of
  UnknownError msg -> classifyError (T.pack msg)
  WontCompile ghcErrors -> 
    -- Take the first GHC error and convert it
    case ghcErrors of
      [] -> CompilationError "Unknown compilation error" defaultLocation []
      (firstErr:_) -> classifyGhcError firstErr
  NotAllowed msg -> SecurityError (T.pack msg)
  GhcException msg -> classifyError (T.pack msg)
  where
    defaultLocation = SourceLocation 1 1 Nothing
    
    -- Convert a GhcError from hint to our GHCError
    classifyGhcError (GhcError errorText) = 
      let errorMsg = T.pack errorText
      in classifyError errorMsg
    
    -- Classify error based on message content
    classifyError msg
      | isTypeError msg = CompilationError msg defaultLocation (generateTypeSuggestions msg)
      | isSyntaxError msg = CompilationError msg defaultLocation (generateSyntaxSuggestions msg)  
      | isNameError msg = CompilationError msg defaultLocation (generateNameSuggestions msg)
      | otherwise = RuntimeError msg
    
    isTypeError msg = any (`T.isInfixOf` T.toLower msg) 
      ["couldn't match expected type", "couldn't match type", "no instance for", "type mismatch"]
    
    isSyntaxError msg = any (`T.isInfixOf` T.toLower msg)
      ["parse error", "syntax error", "unexpected", "missing"]
      
    isNameError msg = any (`T.isInfixOf` T.toLower msg)
      ["not in scope", "variable not in scope", "not defined"]

-- | Convert GHCError to RuntimeDiagnostic for Phase 2 integration
ghcErrorToDiagnostic :: GHCError -> RuntimeDiagnostic
ghcErrorToDiagnostic ghcErr = case ghcErr of
  CompilationError msg location suggestions ->
    mkDiagnostic SeverityError (msg <> " at " <> locationText location)
  RuntimeError msg ->
    mkDiagnostic SeverityError ("Runtime error: " <> msg)
  TimeoutError seconds ->
    mkDiagnostic SeverityError ("Evaluation timed out after " <> T.pack (show seconds) <> " seconds")
  ImportError moduleName msg ->
    mkDiagnostic SeverityError ("Import error for " <> T.pack moduleName <> ": " <> msg)
  SecurityError msg ->
    mkDiagnostic SeverityError ("Security policy violation: " <> msg)
  where
    locationText (SourceLocation line col _) = 
      T.pack (show line) <> ":" <> T.pack (show col)

-- | Enrich diagnostic with additional context and suggestions  
enrichDiagnostic :: GHCError -> RuntimeDiagnostic -> RuntimeDiagnostic
enrichDiagnostic ghcErr diagnostic = diagnostic
  -- For now, return as-is. Will be enhanced in later tasks with suggestion system

-- | Generate helpful suggestions for common errors
generateSuggestions :: GHCError -> [Text]
generateSuggestions ghcErr = case ghcErr of
  CompilationError msg _ _ -> commonErrorSuggestions msg
  ImportError _ _ -> ["Check module name spelling", "Verify module is available"]
  TimeoutError _ -> ["Simplify expression", "Check for infinite loops"]
  _ -> []

-- | Common error suggestions based on error message patterns
commonErrorSuggestions :: Text -> [Text]
commonErrorSuggestions msg
  | "not in scope" `T.isInfixOf` T.toLower msg = 
      ["Check variable spelling", "Import required module", "Define the variable first"]
  | "type mismatch" `T.isInfixOf` T.toLower msg =
      ["Check types match", "Use type conversion functions", "Review function signatures"] 
  | "parse error" `T.isInfixOf` T.toLower msg =
      ["Check syntax", "Balance parentheses", "Check indentation"]
  | otherwise = ["Check Haskell syntax", "Review error message carefully"]

-- | Generate suggestions for type errors
generateTypeSuggestions :: Text -> [Text]
generateTypeSuggestions msg
  | "Char" `T.isInfixOf` msg && "String" `T.isInfixOf` msg =
      ["Use single quotes for Char: 'a'", "Use double quotes for String: \"hello\""]
  | "Integer" `T.isInfixOf` msg && "Int" `T.isInfixOf` msg =
      ["Try using fromInteger or fromIntegral for numeric conversion"]
  | otherwise = commonErrorSuggestions msg

-- | Generate suggestions for syntax errors  
generateSyntaxSuggestions :: Text -> [Text]
generateSyntaxSuggestions msg
  | "unexpected" `T.isInfixOf` T.toLower msg =
      ["Check for missing operators", "Verify parentheses balance", "Check function application"]
  | otherwise = commonErrorSuggestions msg

-- | Generate suggestions for name/scope errors
generateNameSuggestions :: Text -> [Text]
generateNameSuggestions msg
  | "not in scope" `T.isInfixOf` T.toLower msg =
      ["Check spelling", "Import module containing the function", "Define the variable/function"]
  | otherwise = commonErrorSuggestions msg