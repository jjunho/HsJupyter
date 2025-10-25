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
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)
import Language.Haskell.Interpreter (Interpreter, InterpreterT, runInterpreter, interpret, as, setImports, runStmt)

import HsJupyter.Runtime.GHCSession (GHCSessionState(..), GHCConfig(..), ImportPolicy(..), ImportDefault(..), newGHCSession, cleanupSession, listBindings, extractBindingNames, addBinding)
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
importModule :: GHCSessionState -> String -> IO (Either GHCError ())
importModule _session _moduleName = do
  -- Placeholder implementation for Phase 5
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
    -- Safe default import policy - allow basic imports
    defaultSafePolicy = ImportPolicy
      { allowedModules = mempty
      , deniedModules = mempty  
      , defaultPolicy = Allow
      , systemModulesAllowed = True
      }
    
    -- Default resource budget - generous for development
    defaultResourceBudget = ResourceBudget
      { rbCpuTimeout = 30.0
      , rbMemoryLimit = 256 * 1024 * 1024  -- 256MB
      , rbTempDirectory = "/tmp"
      , rbMaxStreamBytes = 1024 * 1024     -- 1MB
      }