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
import Language.Haskell.Interpreter (Interpreter, InterpreterT, runInterpreter, interpret, as, setImports)

import HsJupyter.Runtime.GHCSession (GHCSessionState(..), GHCConfig(..), newGHCSession, cleanupSession, listBindings)
import HsJupyter.Runtime.GHCDiagnostics (GHCError(..), ghcErrorToDiagnostic)
import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic)

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

-- | Evaluate a Haskell expression and return its result
evaluateExpression :: GHCSessionState -> Text -> STM (Either GHCError Text)
evaluateExpression _session code = do
  -- Placeholder implementation - real hint integration will be added in Phase 3
  return $ Right $ "Evaluated: " <> code

-- | Execute a Haskell declaration (variable/function definition)
evaluateDeclaration :: GHCSessionState -> Text -> STM (Either GHCError [String])
evaluateDeclaration _session _code = do
  -- Placeholder implementation
  return $ Right []

-- | Import a Haskell module with security policy checking
importModule :: GHCSessionState -> String -> STM (Either GHCError ())
importModule _session _moduleName = do
  -- Placeholder implementation
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
  { expressionTimeout = 1       -- 1 second for expressions
  , compilationTimeout = 5      -- 5 seconds for imports/compilation
  , computationTimeout = 10     -- 10 seconds for complex computations
  , importPolicy = defaultSafePolicy
  , resourceLimits = defaultResourceBudget
  }
  where
    -- These will be properly imported once available
    defaultSafePolicy = error "ImportPolicy not yet implemented"
    defaultResourceBudget = error "ResourceBudget not yet imported"