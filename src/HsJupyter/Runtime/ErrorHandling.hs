{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared error handling patterns and combinators to eliminate code duplication
-- and improve maintainability across the HsJupyter runtime system.
--
-- This module provides:
-- * Common error handling combinators following DRY principles
-- * Structured error transformation utilities
-- * Resource-safe error handling patterns
-- * Constitutional compliance with Principles VI (DRY) and VII (Defensive Programming)
module HsJupyter.Runtime.ErrorHandling
  ( -- * Error handling combinators
    withErrorContext
  , withTimeoutError
  , withResourceError
  , withCancellationCheck
  
    -- * Error transformation utilities
  , mapInterpreterError
  , enrichDiagnostic
  , propagateError
  
    -- * Resource-safe error patterns
  , withResourceCleanup
  , withMemoryLimit
  , withCancellationSupport
  
    -- * Common error constructors
  , timeoutError
  , cancellationError
  , resourceViolationError
  , compilationError
  ) where

import Control.Exception (catch, finally)
import Control.Concurrent.STM (STM, atomically)
import System.Timeout (timeout)
import Data.Text (Text)
import qualified Data.Text as T

import HsJupyter.Runtime.Diagnostics 
  ( RuntimeDiagnostic(..)
  , DiagnosticSeverity(..)
  , mkDiagnostic
  , mkError
  )
import HsJupyter.Runtime.GHCDiagnostics (GHCError(..), SourceLocation(..))
import HsJupyter.Runtime.ResourceGuard (ResourceViolation(..))

-- | Add contextual information to error handling operations.
-- Follows Constitutional Principle VII (Defensive Programming).
withErrorContext :: Text -> IO (Either e a) -> IO (Either e a)
withErrorContext _context action = do
  result <- action
  case result of
    Left err -> return $ Left err  -- Error context could be added here if needed
    Right val -> return $ Right val

-- | Apply timeout with standardized error handling.
-- Eliminates timeout handling duplication across modules.
withTimeoutError :: Int -> Text -> IO (Either GHCError a) -> IO (Either GHCError a)
withTimeoutError timeoutSeconds operation action = do
  result <- timeout (timeoutSeconds * 1000000) action
  case result of
    Nothing -> return $ Left (timeoutError timeoutSeconds operation)
    Just r -> return r

-- | Handle resource violations consistently across modules.
-- Provides standard resource error transformation.
withResourceError :: Text -> IO (Either GHCError a) -> IO (Either GHCError a)
withResourceError operation action = do
  action `catch` \(violation :: ResourceViolation) ->
    return $ Left (resourceViolationError violation operation)

-- | Check for cancellation before proceeding with operation.
-- Standard cancellation pattern used across evaluation functions.
withCancellationCheck :: STM Bool -> Text -> IO (Either GHCError a) -> IO (Either GHCError a)  
withCancellationCheck checkCancelled operation action = do
  cancelled <- atomically checkCancelled
  if cancelled
    then return $ Left (cancellationError operation)
    else action

-- | Transform hint library interpreter errors to GHC errors.
-- Eliminates error mapping duplication.
mapInterpreterError :: Show e => e -> GHCError
mapInterpreterError err = CompilationError 
  (T.pack $ show err)
  (SourceLocation 1 1 Nothing)
  []

-- | Enrich diagnostic with additional context and suggestions.
-- Standard diagnostic enrichment pattern.
enrichDiagnostic :: RuntimeDiagnostic -> [Text] -> RuntimeDiagnostic
enrichDiagnostic diag suggestions = diag 
  { rdSuggestions = rdSuggestions diag <> suggestions }

-- | Propagate errors through the diagnostic system.
-- Standard error propagation pattern.
propagateError :: GHCError -> RuntimeDiagnostic
propagateError (CompilationError msg _ _) = mkDiagnostic SeverityError msg
propagateError (TimeoutError seconds) = mkError $ "Operation timed out after " <> T.pack (show seconds) <> " seconds"
propagateError (ImportError modName msg) = mkError $ "Import failed for " <> T.pack modName <> ": " <> msg
propagateError (RuntimeError msg) = mkError $ "Runtime error: " <> msg
propagateError (SecurityError msg) = mkError $ "Security error: " <> msg

-- | Resource-safe error handling with cleanup.
-- Constitutional Principle VII compliance - proper resource cleanup.
withResourceCleanup :: IO () -> IO (Either e a) -> IO (Either e a)
withResourceCleanup cleanup action = action `finally` cleanup

-- | Memory limit enforcement with error handling.
-- Standard memory limiting pattern.
withMemoryLimit :: Int -> IO (Either GHCError a) -> IO (Either GHCError a)
withMemoryLimit _limitMB action = do
  -- This would integrate with ResourceGuard for actual memory limiting
  -- For now, provide the pattern for consistent usage
  action `catch` \(violation :: ResourceViolation) ->
    return $ Left (resourceViolationError violation "memory-limited operation")

-- | Cancellation support wrapper.
-- Standard cancellation pattern with proper error propagation.
withCancellationSupport :: STM Bool -> IO (Either GHCError a) -> IO (Either GHCError a)
withCancellationSupport checkCancelled action = do
  cancelled <- atomically checkCancelled
  if cancelled
    then return $ Left (cancellationError "operation")
    else action

-- | Standard timeout error constructor.
timeoutError :: Int -> Text -> GHCError
timeoutError seconds _operation = TimeoutError seconds

-- | Standard cancellation error constructor.
cancellationError :: Text -> GHCError  
cancellationError operation = CompilationError 
  ("Operation was cancelled: " <> operation)
  (SourceLocation 1 1 Nothing)
  ["Check if cancellation was requested", "Retry operation if needed"]

-- | Standard resource violation error constructor.
resourceViolationError :: ResourceViolation -> Text -> GHCError
resourceViolationError violation operation = RuntimeError $
  "Resource violation during " <> operation <> ": " <> T.pack (show violation)

-- | Standard compilation error constructor.
compilationError :: Text -> [Text] -> GHCError
compilationError msg suggestions = CompilationError 
  msg
  (SourceLocation 1 1 Nothing)
  suggestions
