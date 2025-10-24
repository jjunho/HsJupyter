{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Runtime.GHCSession 
  ( -- * Session state types
    GHCSessionState(..)
  , GHCConfig(..)
  , ImportPolicy(..)
  , ImportDefault(..)
    
    -- * State operations
  , newGHCSession
  , addBinding
  , removeBinding
  , listBindings
  , addImportedModule
  , listImportedModules
    
    -- * Session lifecycle
  , withGHCSession
  , cleanupSession
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Haskell.Interpreter (Interpreter, InterpreterT, runInterpreter)

import HsJupyter.Runtime.SessionState (ResourceBudget)

-- | Persistent interpreter state across notebook cell executions
data GHCSessionState = GHCSessionState
  { interpreterInitialized :: Bool              -- Whether interpreter has been set up
  , definedBindings :: TVar (Set String)       -- Thread-safe tracking of defined variables/functions
  , importedModules :: TVar [String]           -- List of successfully imported modules (ModuleName later)
  , sessionConfig :: GHCConfig                 -- Configuration including timeout values and import policy
  } 

-- | Configuration for GHC evaluation behavior and security policies
data GHCConfig = GHCConfig
  { expressionTimeout :: Int          -- Timeout for simple expressions (default: 1 second)
  , compilationTimeout :: Int         -- Timeout for imports/compilation (default: 5 seconds) 
  , computationTimeout :: Int         -- Timeout for complex computations (default: 10 seconds)
  , importPolicy :: ImportPolicy      -- Module import security configuration
  , resourceLimits :: ResourceBudget  -- Integration with existing ResourceGuard
  } deriving (Show, Eq)

-- | Security configuration for controlling module imports
data ImportPolicy = ImportPolicy
  { allowedModules :: Set String      -- Explicitly allowed modules (ModuleName later)
  , deniedModules :: Set String       -- Explicitly denied modules
  , defaultPolicy :: ImportDefault    -- Behavior for unlisted modules (Allow | Deny)
  , systemModulesAllowed :: Bool      -- Whether System.* modules are permitted
  } deriving (Show, Eq)

-- | Default behavior for unlisted modules
data ImportDefault = Allow | Deny deriving (Show, Eq)

-- | Create a new GHC session with default configuration
newGHCSession :: GHCConfig -> STM GHCSessionState
newGHCSession config = do
  bindings <- newTVar Set.empty
  modules <- newTVar []
  return $ GHCSessionState
    { interpreterInitialized = False  -- Will be initialized when first used
    , definedBindings = bindings
    , importedModules = modules
    , sessionConfig = config
    }

-- | Add a new binding to the session state
addBinding :: GHCSessionState -> String -> STM ()
addBinding session name = modifyTVar' (definedBindings session) (Set.insert name)

-- | Remove a binding from the session state
removeBinding :: GHCSessionState -> String -> STM ()
removeBinding session name = modifyTVar' (definedBindings session) (Set.delete name)

-- | Get list of currently defined bindings
listBindings :: GHCSessionState -> STM [String]
listBindings session = Set.toList <$> readTVar (definedBindings session)

-- | Add a successfully imported module
addImportedModule :: GHCSessionState -> String -> STM ()
addImportedModule session moduleName = modifyTVar' (importedModules session) (moduleName:)

-- | Get list of imported modules
listImportedModules :: GHCSessionState -> STM [String]
listImportedModules session = readTVar (importedModules session)

-- | Execute action with managed GHC session
withGHCSession :: GHCConfig -> (GHCSessionState -> STM a) -> STM (Either String a)
withGHCSession config action = do
  session <- newGHCSession config
  result <- action session
  return $ Right result  -- Error handling will be improved in later tasks

-- | Cleanup session resources
cleanupSession :: GHCSessionState -> STM ()
cleanupSession session = do
  writeTVar (definedBindings session) Set.empty
  writeTVar (importedModules session) []