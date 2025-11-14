{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Runtime.GHCDiagnostics
  ( -- * Error types
    GHCError(..)
  , GHCErrorType(..)
  , SourceLocation(..)
  , SyntaxErrorType(..)
    
    -- * Error conversion
  , interpretError
  , ghcErrorToDiagnostic
  , enrichDiagnostic
    
    -- * Error detection and parsing
  , extractSourceLocation
  , detectSyntaxErrorType
  , extractUndefinedVariable
    
    -- * Suggestion system
  , generateSuggestions
  , commonErrorSuggestions
  , generateTypeSuggestions
  , generateNameSuggestions
  , generateEnhancedSyntaxSuggestions
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
          location = extractSourceLocation errorMsg
      in classifyErrorWithLocation errorMsg location
    
    -- Classify error based on message content (fallback when no location available)
    classifyError msg = classifyErrorWithLocation msg defaultLocation
    
    -- Classify error with location information
    classifyErrorWithLocation msg location
      | isTypeError msg = CompilationError msg location (generateTypeSuggestions msg)
      | isSyntaxError msg = CompilationError msg location (generateEnhancedSyntaxSuggestions msg)  
      | isNameError msg = CompilationError msg location (generateNameSuggestions msg)
      | otherwise = RuntimeError msg
    
    isTypeError msg = any (`T.isInfixOf` T.toLower msg) 
      ["couldn't match expected type", "couldn't match type", "no instance for", "type mismatch"]
    
    isSyntaxError msg = any (`T.isInfixOf` T.toLower msg)
      [ "parse error", "syntax error", "unexpected", "missing"
      , "lexical error", "illegal character", "unterminated"
      , "expecting", "found", "indentation", "layout"
      , "bracket", "parenthesis", "brace", "quote"
      ]
      
    isNameError msg = any (`T.isInfixOf` T.toLower msg)
      ["not in scope", "variable not in scope", "not defined", "undefined", "not bound"]

-- | Extract undefined variable name from error message for better suggestions
extractUndefinedVariable :: Text -> Maybe Text
extractUndefinedVariable msg
  | "Variable not in scope:" `T.isInfixOf` msg = extractAfterColon msg
  | "Not in scope:" `T.isInfixOf` msg = extractAfterColon msg
  | "undefined" `T.isInfixOf` T.toLower msg = extractQuotedName msg
  | otherwise = Nothing
  where
    extractAfterColon txt =
      case T.splitOn ":" txt of
        (_:rest) -> case rest of
          (name:_) -> Just $ sanitizeName $ T.takeWhile (/= ' ') $ T.strip name
          [] -> Nothing
        [] -> Nothing

    extractQuotedName txt =
      case T.splitOn "'" txt of
        (_:name:_) -> Just $ sanitizeName $ T.takeWhile (/= '\'') name
        _ -> Nothing

    -- Remove surrounding quotes or backticks if present
    sanitizeName t =
      let s = T.strip t
      in case (T.uncons s, T.unsnoc s) of
        (Just ('\'', _), Just (_, '\'')) -> T.init $ T.tail s
        (Just ('"', _), Just (_, '"')) -> T.init $ T.tail s
        (Just ('`', _), Just (_, '`')) -> T.init $ T.tail s
        _ -> s

-- | Enhanced syntax error detection with specific patterns
detectSyntaxErrorType :: Text -> Maybe SyntaxErrorType
detectSyntaxErrorType msg
  | any (`T.isInfixOf` lowerMsg) ["unterminated string", "lexical error in string"] = 
      Just UnterminatedString
  | any (`T.isInfixOf` lowerMsg) ["expecting", "expected"] && "found" `T.isInfixOf` lowerMsg =
      Just UnexpectedToken
  | any (`T.isInfixOf` lowerMsg) ["indentation", "layout"] =
      Just IndentationError  
  | any (`T.isInfixOf` lowerMsg) ["bracket", "parenthesis", "brace"] =
      Just UnbalancedDelimiters
  | "missing" `T.isInfixOf` lowerMsg =
      Just MissingToken
  | otherwise = Nothing
  where
    lowerMsg = T.toLower msg

-- | Specific syntax error types for targeted suggestions
data SyntaxErrorType
  = UnterminatedString
  | UnexpectedToken
  | IndentationError
  | UnbalancedDelimiters
  | MissingToken
  deriving (Show, Eq)

-- | Extract source location from GHC error message
extractSourceLocation :: Text -> SourceLocation
extractSourceLocation msg = 
  case parseLocationFromMessage msg of
    Just loc -> loc
    Nothing -> SourceLocation 1 1 Nothing
  where
    parseLocationFromMessage txt
      -- Match patterns like "file.hs:5:12:" or "<interactive>:3:7:"
      | Just (line, col) <- extractLineColumn txt = Just $ SourceLocation line col Nothing
      | otherwise = Nothing
    
    extractLineColumn txt =
      let cleaned = T.replace "<interactive>" "" txt
          parts = T.splitOn ":" cleaned
      in case parts of
        (_:lineStr:colStr:_) -> do
          line <- readMaybe (T.unpack $ T.strip lineStr)
          col <- readMaybe (T.unpack $ T.strip colStr)
          return (line, col)
        _ -> Nothing
    
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- | Convert GHCError to RuntimeDiagnostic for Phase 2 integration
ghcErrorToDiagnostic :: GHCError -> RuntimeDiagnostic
ghcErrorToDiagnostic ghcErr = case ghcErr of
  CompilationError msg location _ ->
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
enrichDiagnostic ghcErr diagnostic = 
  let suggestions = case ghcErr of
        CompilationError _ _ existing -> existing ++ generateSuggestions ghcErr
        _ -> generateSuggestions ghcErr
  in diagnostic { rdSuggestions = rdSuggestions diagnostic ++ suggestions }

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

-- | Generate enhanced suggestions for type errors with expected/actual types
generateTypeSuggestions :: Text -> [Text]
generateTypeSuggestions msg
  | "Char" `T.isInfixOf` msg && "String" `T.isInfixOf` msg =
      ["Use single quotes for Char: 'a'", "Use double quotes for String: \"hello\""]
  | "Integer" `T.isInfixOf` msg && "Int" `T.isInfixOf` msg =
      ["Try using fromInteger or fromIntegral for numeric conversion"]
  | "Couldn't match expected type" `T.isInfixOf` msg =
      extractTypeErrorSuggestions msg
  | "No instance for" `T.isInfixOf` msg =
      ["Add missing type class instance", "Import required module", "Check type constraints"]
  | otherwise = commonErrorSuggestions msg

-- | Extract expected and actual types from error message for targeted suggestions
extractTypeErrorSuggestions :: Text -> [Text]
extractTypeErrorSuggestions msg
  | "Bool" `T.isInfixOf` msg && ("Int" `T.isInfixOf` msg || "Integer" `T.isInfixOf` msg) =
      ["Use comparison operators (==, <, >) for Bool results", "Use if-then-else for conditional values"]
  | "[" `T.isInfixOf` msg && "Char" `T.isInfixOf` msg =
      ["String is [Char] - they are the same type", "Use string operations or convert explicitly"]
  | "IO" `T.isInfixOf` msg =
      ["Use <- in do notation for IO actions", "Use return to wrap pure values in IO"]
  | "Maybe" `T.isInfixOf` msg =
      ["Use case analysis or fromMaybe", "Check for Nothing values", "Use fmap or <$> for Maybe values"]
  | otherwise = 
      ["Check function types match", "Use type annotations to clarify", "Consider type conversion functions"]

-- | Generate enhanced suggestions for syntax errors based on error type
generateEnhancedSyntaxSuggestions :: Text -> [Text]
generateEnhancedSyntaxSuggestions msg = 
  case detectSyntaxErrorType msg of
    Just UnterminatedString -> 
      ["Close the string with matching quotes", "Check for escaped quotes", "Use multi-line strings if needed"]
    Just UnexpectedToken ->
      ["Check for missing operators", "Verify parentheses balance", "Check function application syntax"]
    Just IndentationError ->
      ["Check indentation alignment", "Use consistent spacing", "Align with previous line"]
    Just UnbalancedDelimiters ->
      ["Balance parentheses, brackets, or braces", "Check nested expressions", "Use editor bracket matching"]
    Just MissingToken ->
      ["Add missing operator or delimiter", "Check function syntax", "Complete the expression"]
    Nothing -> generateSyntaxSuggestions msg

-- | Generate suggestions for syntax errors (fallback)
generateSyntaxSuggestions :: Text -> [Text]
generateSyntaxSuggestions msg
  | "unexpected" `T.isInfixOf` T.toLower msg =
      ["Check for missing operators", "Verify parentheses balance", "Check function application"]
  | otherwise = commonErrorSuggestions msg

-- | Generate enhanced suggestions for name/scope errors
generateNameSuggestions :: Text -> [Text]
generateNameSuggestions msg
  | "not in scope" `T.isInfixOf` T.toLower msg = 
      case extractUndefinedVariable msg of
        Just varName -> generateVariableSuggestions varName
        Nothing -> defaultNameSuggestions
  | otherwise = commonErrorSuggestions msg
  where
    defaultNameSuggestions = 
      ["Check spelling", "Import module containing the function", "Define the variable/function"]
    
    generateVariableSuggestions varName =
      [ "Check spelling of '" <> varName <> "'"
      , "Import module containing '" <> varName <> "'"
      , "Define '" <> varName <> "' before using it"
      , "Check if '" <> varName <> "' is in scope"
      ] ++ getCommonMisspellingSuggestions varName
    
    getCommonMisspellingSuggestions varName
      | varName == "lenght" = ["Did you mean 'length'?"]
      | varName == "fiter" = ["Did you mean 'filter'?"]
      | varName == "mapp" = ["Did you mean 'map'?"]
      | varName == "foldr1" && "foldr" `T.isInfixOf` msg = ["Did you mean 'foldr'?"]
      | otherwise = []
