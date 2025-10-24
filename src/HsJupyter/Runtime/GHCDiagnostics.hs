{-# LANGUAGE OverloadedStrings #-}

-- | Error handling and diagnostic reporting for GHC evaluation
-- Maps GHC errors to the existing RuntimeDiagnostic system
module HsJupyter.Runtime.GHCDiagnostics where

import Data.Text (Text)

-- Placeholder module structure - will be implemented in Phase 2
-- TODO: Implement GHC error mapping and diagnostic enrichment

-- | Basic GHC error type placeholder
data GHCError
  = CompilationError Text
  | RuntimeError Text
  deriving (Show, Eq)

-- | Basic error handling placeholder
formatGHCError :: GHCError -> Text
formatGHCError (CompilationError msg) = "Compilation Error: " <> msg
formatGHCError (RuntimeError msg) = "Runtime Error: " <> msg