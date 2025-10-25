{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module HsJupyter.Runtime.SessionState
  ( ExecutionStatus(..)
  , StreamName(..)
  , StreamChunk(..)
  , JobMetadata(..)
  , ExecuteContext(..)
  , ExecutionJob(..)
  , JobType(..)
  , RuntimeSessionState(..)
  , ModuleArtifact(..)
  , Binding(..)
  , ResourceBudget(..)
  , ExecutionOutcome(..)
  , initialSessionState
  , incrementExecutionCount
  , enqueueable
  ) where

import Control.Concurrent.STM.TMVar (TMVar)
import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import GHC.Generics (Generic)

import HsJupyter.Runtime.Diagnostics (RuntimeDiagnostic)

-- | Status values returned after running an execution job.
data ExecutionStatus
  = ExecutionOk
  | ExecutionError
  | ExecutionAbort
  | ExecutionResourceLimit
  deriving stock (Eq, Show, Generic)

-- | Named stream channels supported by the runtime.
data StreamName = StreamStdout | StreamStderr
  deriving stock (Eq, Show, Generic)

-- | Chunk of output emitted during execution.
data StreamChunk = StreamChunk
  { streamName :: StreamName
  , streamText :: Text
  }
  deriving stock (Eq, Show, Generic)

-- | Metadata supplied alongside an execute_request.
data JobMetadata = JobMetadata
  { jmSilent         :: Bool
  , jmStoreHistory   :: Bool
  , jmAllowStdin     :: Bool
  , jmUserExpressions :: Value
  }
  deriving stock (Eq, Show, Generic)

-- | Context extracted from the Jupyter message header.
data ExecuteContext = ExecuteContext
  { ecMessageId   :: Text
  , ecSessionId   :: Text
  , ecUsername    :: Text
  , ecParentId    :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- | Representation of a queued execution job.
data ExecutionJob = ExecutionJob
  { jobContext     :: ExecuteContext
  , jobSource      :: Text
  , jobSubmittedAt :: UTCTime
  , jobMetadata    :: JobMetadata
  , jobCancelToken :: TMVar ()
  , jobType        :: JobType  -- NEW: distinguish between Echo and GHC jobs
  }
  deriving stock Generic

-- | Type of job execution (extending for GHC evaluation)
data JobType
  = EchoJob     -- Original echo-based evaluation
  | GHCJob      -- NEW: GHC-based Haskell evaluation
  deriving stock (Eq, Show, Generic)

instance Show ExecutionJob where
  show job = "ExecutionJob {msgId=" ++ show (ecMessageId (jobContext job)) ++ "}"

-- | Information persisted between notebook executions.
data RuntimeSessionState = RuntimeSessionState
  { rssLoadedModules :: Map Text ModuleArtifact
  , rssImports       :: [Text]
  , rssBindings      :: Map Text Binding
  , rssExecutionCount :: Int
  , rssResourceBudget :: ResourceBudget
  }
  deriving stock (Eq, Show, Generic)

-- | Structured result returned after job completion.
data ExecutionOutcome = ExecutionOutcome
  { outcomeStatus        :: ExecutionStatus
  , outcomeStreams       :: [StreamChunk]
  , outcomePayload       :: [Value]
  , outcomeDiagnostics   :: [RuntimeDiagnostic]
  , outcomeExecutionCount :: Int
  , outcomeDuration      :: NominalDiffTime
  }
  deriving stock (Eq, Show, Generic)

-- | Compiled artifacts retained for reuse.
data ModuleArtifact = ModuleArtifact
  { maObjectPath    :: FilePath
  , maInterfacePath :: Maybe FilePath
  , maHash          :: Text
  }
  deriving stock (Eq, Show, Generic)

-- | Lightweight binding metadata for debugging and telemetry.
data Binding = Binding
  { bindingName :: Text
  , bindingType :: Text
  }
  deriving stock (Eq, Show, Generic)

-- | Resource guard configuration applied per session/job.
data ResourceBudget = ResourceBudget
  { rbCpuTimeout    :: NominalDiffTime
  , rbMemoryLimit   :: Int64
  , rbTempDirectory :: FilePath
  , rbMaxStreamBytes :: Int64
  }
  deriving stock (Eq, Show, Generic)

-- | Initialise a blank session state using the supplied resource budget.
initialSessionState :: ResourceBudget -> RuntimeSessionState
initialSessionState budget =
  RuntimeSessionState
    { rssLoadedModules = Map.empty
    , rssImports = []
    , rssBindings = Map.empty
    , rssExecutionCount = 0
    , rssResourceBudget = budget
    }

-- | Increment the execution count after a successful job.
incrementExecutionCount :: RuntimeSessionState -> RuntimeSessionState
incrementExecutionCount state =
  state { rssExecutionCount = rssExecutionCount state + 1 }

-- | Check whether the queue is within capacity limits (placeholder for now).
enqueueable :: Int -> Int -> Bool
enqueueable capacity queuedJobs = queuedJobs < capacity
