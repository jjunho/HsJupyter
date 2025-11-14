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
  , addDeclaration
  , listDeclarations
  , addImportedModule
  , listImportedModules
  , recordDeclaration
  , listDeclarations
  , extractBindingNames
    
    -- * Import policy checking
  , checkImportPolicy
  , isModuleAllowed
  , defaultSafeModules
    
    -- * Session lifecycle
  , withGHCSession
  , cleanupSession
  ) where

import Control.Concurrent.STM
import Data.List (isPrefixOf, isInfixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import HsJupyter.Runtime.SessionState (ResourceBudget)

-- Forward declaration for import policy checking
-- Note: This creates a circular import issue, will be resolved in next task

-- | Persistent interpreter state across notebook cell executions
data GHCSessionState = GHCSessionState
  { sessionId :: String                        -- Unique session identifier
  , definedBindings :: TVar (Set String)       -- Thread-safe tracking of defined variables/functions
  , declarations :: TVar [String]              -- Full declaration code for replay
  , importedModules :: TVar [String]           -- List of successfully imported modules (full import statements)
  , sessionConfig :: GHCConfig                 -- Configuration including timeout values and import policy
  , interpreterState :: TVar InterpreterState  -- Current interpreter state
  , sessionDeclarations :: TVar [Text]         -- Record of declarations to replay in fresh interpreters
  }

-- | State of the hint interpreter
data InterpreterState 
  = NotInitialized                             -- No interpreter created yet
  | Initialized                                -- Interpreter ready for use
  | Failed String                              -- Interpreter failed to initialize
  deriving (Show, Eq) 

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
  decls <- newTVar []
  modules <- newTVar []
  interpreterSt <- newTVar NotInitialized
  declarations <- newTVar []
  return $ GHCSessionState
    { sessionId = "default-session"  -- TODO: Generate unique session ID
    , definedBindings = bindings
    , declarations = decls
    , importedModules = modules
    , sessionConfig = config
    , interpreterState = interpreterSt
    , sessionDeclarations = declarations
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

-- | Add a declaration to the session state (for replay)
addDeclaration :: GHCSessionState -> String -> STM ()
addDeclaration session decl = modifyTVar' (declarations session) (++ [decl])

-- | Get list of all declarations for replay
listDeclarations :: GHCSessionState -> STM [String]
listDeclarations session = readTVar (declarations session)

-- | Add a successfully imported module (full import statement)
addImportedModule :: GHCSessionState -> String -> STM ()
addImportedModule session moduleName =
  modifyTVar' (importedModules session) $ \mods ->
    moduleName : filter (/= moduleName) mods

-- | Get list of imported modules
listImportedModules :: GHCSessionState -> STM [String]
listImportedModules session = readTVar (importedModules session)

-- | Record a declaration so it can be replayed in subsequent interpreter sessions
recordDeclaration :: GHCSessionState -> Text -> STM ()
recordDeclaration session decl = modifyTVar' (sessionDeclarations session) (++ [decl])

-- | Retrieve the list of previously recorded declarations
listDeclarations :: GHCSessionState -> STM [Text]
listDeclarations session = readTVar (sessionDeclarations session)

-- | Execute action with managed GHC session
withGHCSession :: GHCConfig -> (GHCSessionState -> STM a) -> STM (Either String a)
withGHCSession config action = do
  session <- newGHCSession config
  result <- action session
  return $ Right result  -- Error handling will be improved in later tasks

-- | Extract binding names from Haskell declaration code
extractBindingNames :: Text -> [String]
extractBindingNames code = 
  let codeStr = T.unpack $ T.strip code
      lines' = lines codeStr
  in concatMap extractFromLine lines'
  where
    extractFromLine line
      | "let " `isPrefixOf` (dropWhile (== ' ') line) = 
          extractLetBinding (dropWhile (== ' ') line)
      | any (`elem` line) ['='] && not ("==" `isInfixOf` line) && not ("=>" `isInfixOf` line) =
          extractFunctionBinding line
      | otherwise = []
    
    extractLetBinding line = 
      case words (drop 4 line) of  -- drop "let "
        (name:_) -> [takeWhile (\c -> c /= '=' && c /= ' ') name]
        [] -> []
    
    extractFunctionBinding line =
      case words line of
        (name:_) -> [takeWhile (\c -> c /= '=' && c /= ' ' && c /= '(') name]
        [] -> []
    
    -- elem function is already defined in Prelude

-- | Check if a module import is allowed by the security policy
checkImportPolicy :: GHCSessionState -> String -> STM (Either String ())
checkImportPolicy session moduleName = do
  let policy = importPolicy (sessionConfig session)
  return $ if isModuleAllowed policy moduleName
    then Right ()
    else Left $ "Module import denied by security policy: " ++ moduleName

-- | Determine if a module is allowed by the import policy
isModuleAllowed :: ImportPolicy -> String -> Bool
isModuleAllowed policy moduleName
  -- Check explicit deny list first (highest priority)
  | moduleName `Set.member` deniedModules policy = False
  -- Check explicit allow list
  | moduleName `Set.member` allowedModules policy = True
  -- Check system modules policy
  | "System." `isPrefixOf` moduleName = systemModulesAllowed policy
  -- Check if it's in the default safe modules
  | moduleName `Set.member` defaultSafeModules = True
  -- Apply default policy
  | otherwise = case defaultPolicy policy of
      Allow -> True
      Deny -> False

-- | Default set of safe modules that are always allowed
defaultSafeModules :: Set String
defaultSafeModules = Set.fromList
  [ "Prelude"
  , "Data.List"
  , "Data.Maybe"
  , "Data.Either"
  , "Data.Bool"
  , "Data.Char"
  , "Data.Function"
  , "Data.Tuple"
  , "Control.Monad"
  , "Control.Applicative"
  , "Control.Arrow"
  , "Text.Show"
  , "Text.Read"
  , "Numeric"
  ]

-- | Cleanup session resources
cleanupSession :: GHCSessionState -> STM ()
cleanupSession session = do
  writeTVar (definedBindings session) Set.empty
  writeTVar (importedModules session) []
  writeTVar (sessionDeclarations session) []
