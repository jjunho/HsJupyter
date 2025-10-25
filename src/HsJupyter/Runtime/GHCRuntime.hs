{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Runtime.GHCRuntime
  ( -- * Core evaluation functions
    evaluateExpression
  , evaluateDeclaration  
  , importModule
    
    -- * Session management
  , initializeGHCSession
  , resetGHCSession
  , getSessionBindings
    
    -- * Configuration
  , GHCEvaluationRequest(..)
  , GHCEvaluationResult(..)
  , EvaluationType(..)
  , defaultGHCConfig
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)
import Language.Haskell.Interpreter (Interpreter, InterpreterT, runInterpreter, interpret, as, setImports, runStmt)

import HsJupyter.Runtime.GHCSession (GHCSessionState(..), GHCConfig(..), ImportPolicy(..), ImportDefault(..), newGHCSession, cleanupSession, listBindings, extractBindingNames, addBinding, addImportedModule, listImportedModules, checkImportPolicy, defaultSafeModules)
import HsJupyter.Runtime.GHCDiagnostics (GHCError(..), ghcErrorToDiagnostic, interpretError)
import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic)
import HsJupyter.Runtime.SessionState (ResourceBudget(..))

-- | Input structure for GHC evaluation operations
data GHCEvaluationRequest = GHCEvaluationRequest
  { reqCode :: Text           -- Haskell code to evaluate
  , reqType :: EvaluationType -- Expression | Declaration | Import
  , reqSessionId :: Text      -- Identifies the target session
  , reqTimeout :: Maybe Int   -- Optional custom timeout
  } deriving (Show, Eq)

-- | Output structure for GHC evaluation results
data GHCEvaluationResult = GHCEvaluationResult
  { resSuccess :: Bool            -- Whether evaluation succeeded
  , resResult :: Maybe Text       -- Evaluated result for expressions
  , resOutput :: Text            -- Stdout/stderr output
  , resDiagnostics :: [RuntimeDiagnostic] -- Errors, warnings, info messages
  , resBindingsAdded :: [String]  -- New variable/function bindings created
  , resModulesImported :: [String] -- New modules successfully imported
  } deriving (Show, Eq)

-- | Type of evaluation to perform
data EvaluationType
  = Expression      -- Return computed value
  | Declaration     -- Define variable/function
  | Import         -- Import module
  deriving (Show, Eq)

-- | Evaluate a Haskell expression with timeout protection
evaluateExpression :: GHCSessionState -> Text -> IO (Either GHCError Text)
evaluateExpression session code = do
  let timeoutSeconds = expressionTimeout (sessionConfig session)
  result <- timeout (timeoutSeconds * 1000000) $ runInterpreter $ do  -- Convert to microseconds
    setImports ["Prelude"]  -- Start with basic Prelude imports
    -- Wrap expression with 'show' to get String representation  
    let wrappedCode = "show (" ++ T.unpack code ++ ")"
    interpret wrappedCode (as :: String)
  case result of
    Nothing -> return $ Left (TimeoutError timeoutSeconds)  -- Timeout occurred
    Just (Left err) -> return $ Left (interpretError err)   -- Interpreter error
    Just (Right value) -> return $ Right (T.pack value)     -- Success

-- | Execute a Haskell declaration (variable/function definition)
evaluateDeclaration :: GHCSessionState -> Text -> IO (Either GHCError [String])
evaluateDeclaration session code = do
  let timeoutSeconds = compilationTimeout (sessionConfig session)
  result <- timeout (timeoutSeconds * 1000000) $ runInterpreter $ do  -- Convert to microseconds
    setImports ["Prelude"]  -- Start with basic Prelude imports
    -- Execute the declaration using runStmt (for let bindings, function definitions)
    runStmt (T.unpack code)
  case result of
    Nothing -> return $ Left (TimeoutError timeoutSeconds)  -- Timeout occurred
    Just (Left err) -> return $ Left (interpretError err)   -- Interpreter error
    Just (Right _) -> do
      -- Extract binding names and update session state
      let bindingNames = extractBindingNames code
      atomically $ mapM_ (addBinding session) bindingNames
      return $ Right bindingNames

-- | Import a Haskell module with security policy checking
-- Supports: import ModuleName, import qualified ModuleName, import qualified ModuleName as Alias
importModule :: GHCSessionState -> String -> IO (Either GHCError ())
importModule session importStatement = do
  -- Validate import statement first
  case validateImportStatement importStatement of
    Left err -> return $ Left (ImportError "invalid" (T.pack err))
    Right () -> do
      -- Parse import statement to extract module name
      let (moduleName, isQualified) = parseImportStatement importStatement
      
      -- Check if module is allowed by policy
      policyCheck <- atomically $ checkImportPolicy session moduleName
      case policyCheck of
        Left err -> return $ Left (ImportError moduleName (T.pack err))
        Right () -> do
          -- Execute import with timeout protection
          let timeoutSeconds = compilationTimeout (sessionConfig session)
          result <- timeout (timeoutSeconds * 1000000) $ runInterpreter $ do
            -- Get current imports and add the new module
            currentImports <- liftIO $ atomically $ listImportedModules session
            let allImports = "Prelude" : currentImports ++ [importStatement]
            setImports allImports
            return ()
          
          case result of
            Nothing -> return $ Left (TimeoutError timeoutSeconds)
            Just (Left err) -> return $ Left (interpretError err)
            Just (Right ()) -> do
              -- Add to imported modules list (store the full import statement)
              atomically $ addImportedModule session importStatement
              return $ Right ()

-- | Initialize a new GHC session with configuration
initializeGHCSession :: GHCConfig -> STM (Either GHCError GHCSessionState)
initializeGHCSession config = do
  session <- newGHCSession config
  return $ Right session

-- | Reset session state while preserving interpreter instance
resetGHCSession :: GHCSessionState -> STM ()
resetGHCSession session = cleanupSession session

-- | Get list of currently defined bindings
getSessionBindings :: GHCSessionState -> STM [String]
getSessionBindings = listBindings

-- | Import statement components
data ImportStatement = ImportStatement
  { importModuleName :: String
  , importQualified :: Bool
  , importAlias :: Maybe String
  , importList :: Maybe [String]  -- Nothing = import all, Just [] = import nothing, Just [items] = selective
  } deriving (Show, Eq)

-- | Parse import statement to extract module name and qualification info
parseImportStatement :: String -> (String, Bool)
parseImportStatement stmt = 
  let parsed = parseImportStatementFull stmt
  in (importModuleName parsed, importQualified parsed)

-- | Parse import statement into full structure
parseImportStatementFull :: String -> ImportStatement
parseImportStatementFull stmt = 
  let cleanStmt = dropWhile (== ' ') stmt
      tokens = words cleanStmt
  in case tokens of
    -- import qualified ModuleName
    ["import", "qualified", moduleName] -> 
      ImportStatement moduleName True Nothing Nothing
    -- import qualified ModuleName as Alias
    ["import", "qualified", moduleName, "as", alias] -> 
      ImportStatement moduleName True (Just alias) Nothing
    -- import ModuleName
    ["import", moduleName] -> 
      ImportStatement moduleName False Nothing Nothing
    -- import ModuleName as Alias
    ["import", moduleName, "as", alias] -> 
      ImportStatement moduleName False (Just alias) Nothing
    -- import ModuleName (selective imports)
    ("import":moduleName:rest) -> 
      let (selective, alias) = parseSelectiveAndAlias (unwords rest)
      in ImportStatement moduleName False alias selective
    -- import qualified ModuleName (selective imports)
    ("import":"qualified":moduleName:rest) -> 
      let (selective, alias) = parseSelectiveAndAlias (unwords rest)
      in ImportStatement moduleName True alias selective
    -- Just the module name (assume simple import)
    [moduleName] -> 
      ImportStatement moduleName False Nothing Nothing
    -- Default case - treat as module name
    _ -> 
      ImportStatement stmt False Nothing Nothing

-- | Parse selective imports and alias from the rest of import statement
parseSelectiveAndAlias :: String -> (Maybe [String], Maybe String)
parseSelectiveAndAlias rest
  | null rest = (Nothing, Nothing)
  | "(" `isPrefixOf` rest && ")" `isInfixOf` rest = 
      let (selective, remainder) = parseSelectiveImports rest
          alias = parseAliasFromRemainder remainder
      in (selective, alias)
  | " as " `isInfixOf` rest = 
      (Nothing, parseAliasFromRemainder rest)
  | otherwise = (Nothing, Nothing)
  where
    parseSelectiveImports :: String -> (Maybe [String], String)
    parseSelectiveImports str = 
      case break (== ')') str of
        (beforeParen, afterParen) ->
          let itemsStr = drop 1 beforeParen  -- Remove opening '('
              items = map (filter (/= ' ')) $ splitOn ',' itemsStr
              remainder = drop 1 afterParen  -- Remove closing ')'
          in (Just items, remainder)
    
    parseAliasFromRemainder :: String -> Maybe String
    parseAliasFromRemainder str
      | " as " `isInfixOf` str = 
          case words str of
            (_:"as":alias:_) -> Just alias
            _ -> Nothing
      | otherwise = Nothing
    
    splitOn :: Char -> String -> [String]
    splitOn _ "" = []
    splitOn delim str = 
      case break (== delim) str of
        (before, "") -> [before]
        (before, _:after) -> before : splitOn delim after
    


-- | Validate import statement syntax and security
validateImportStatement :: String -> Either String ()
validateImportStatement stmt
  | null stmt = Left "Empty import statement"
  | length stmt > 200 = Left "Import statement too long"
  | any (`elem` stmt) [';', '&', '|', '`'] = Left "Invalid characters in import statement"
  | otherwise = Right ()

-- | Default GHC configuration with safe defaults
defaultGHCConfig :: GHCConfig
defaultGHCConfig = GHCConfig
  { expressionTimeout = 10      -- 10 seconds for expressions (increased for reliability)
  , compilationTimeout = 15     -- 15 seconds for imports/compilation
  , computationTimeout = 20     -- 20 seconds for complex computations
  , importPolicy = defaultSafePolicy
  , resourceLimits = defaultResourceBudget
  }
  where
    -- Safe default import policy - allow safe modules, deny system modules by default
    defaultSafePolicy = ImportPolicy
      { allowedModules = defaultSafeModules
      , deniedModules = mempty  
      , defaultPolicy = Deny
      , systemModulesAllowed = False
      }
    
    -- Default resource budget - generous for development
    defaultResourceBudget = ResourceBudget
      { rbCpuTimeout = 30.0
      , rbMemoryLimit = 256 * 1024 * 1024  -- 256MB
      , rbTempDirectory = "/tmp"
      , rbMaxStreamBytes = 1024 * 1024     -- 1MB
      }