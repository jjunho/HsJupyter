{-# LANGUAGE OverloadedStrings #-}

-- | Session state management for GHC evaluation
-- Maintains persistent interpreter state across notebook cell executions
module HsJupyter.Runtime.GHCSession where

import Control.Concurrent.STM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- Placeholder module structure - will be implemented in Phase 2
-- TODO: Implement full GHCSessionState and session management

-- | Basic session state placeholder
data GHCSessionState = GHCSessionState
  { sessionActive :: TVar Bool
  } deriving ()

-- | Create new empty session state
newGHCSession :: STM GHCSessionState
newGHCSession = do
  active <- newTVar True
  return $ GHCSessionState active