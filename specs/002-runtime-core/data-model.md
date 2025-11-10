
# Data Model: Phase 2 – Runtime Core

## RuntimeSessionState

- **Fields**:
  - `loadedModules :: Map ModuleName ModuleArtifact` — cache of compiled modules and artifacts.
  - `imports :: [ImportDecl]` — imports carried between cells.
  - `context :: [Binding]` — top-level definitions (names, types, values) available for subsequent executions.
  - `executionCount :: Int` — monotonically increasing count per session.
  - `resourceBudget :: ResourceBudget` — active guard thresholds for CPU/memory/time.
- **Relationships**: Owned by `Runtime.Manager`; mutated by evaluation helpers; serialized for telemetry snapshots.
- **Validation Rules**: Must remain consistent after each job (no duplicate module names, executionCount >= previous).

## ExecutionJob

- **Fields**:
  - `jobId :: Text` — unique id (msg_id) for correlation.
  - `source :: Text` — cell content.
  - `metadata :: JobMetadata` — e.g., silent flag, store_history, user expressions.
  - `submittedAt :: UTCTime` — enqueued timestamp.
  - `cancelToken :: TMVar ()` — signal used to cancel evaluation when interrupt arrives.
  - `clientInfo :: ExecuteContext` — session identifiers (username, session-id, parent header).
- **Relationships**: Enqueued in `TBQueue ExecutionJob`, consumed by runtime worker.
- **Validation Rules**: `source` length must be below payload guard; `cancelToken` initialised empty.

## ExecutionOutcome

- **Fields**:
  - `status :: ExecuteStatus` — `Ok`, `Error`, `Abort`, `ResourceLimit`.
  - `streams :: [StreamChunk]` — stdout/stderr frames with order preserved.
  - `payload :: [Value]` — rich data for display_data/execute_result.
  - `diagnostics :: [RuntimeDiagnostic]` — structured errors/warnings.
  - `executionCount :: Int` — new count returned to clients.
  - `duration :: NominalDiffTime` — wall-clock runtime for telemetry.
- **Relationships**: Produced by runtime worker, converted to protocol envelopes by bridge/router.
- **Validation Rules**: `executionCount` increments by 1 per successful/abort outcome; `ResourceLimit` outcomes must include at least one diagnostic entry.

## ResourceBudget

- **Fields**:
  - `cpuTimeout :: NominalDiffTime` — maximum execution duration.
  - `memoryLimit :: Bytes` — soft memory cap.
  - `tempDir :: FilePath` — sandbox directory for compiled artifacts.
  - `maxStreamBytes :: Int64` — per-message stream guard.
- **Relationships**: Configured at session start; consumed by guard/watcher modules.
- **Validation Rules**: All values must be positive; `tempDir` must exist/be writable before execution starts.

## Diagnostics & Supporting Types

- **RuntimeDiagnostic**: severity (error/warning/info), summary, optional file/span, suggestions.
- **ModuleArtifact**: path to compiled object, interface, digest for caching.
- **JobMetadata**: silent/store_history/user_expressions flags supplied by frontend.
- **ExecutionQueue**: `TBQueue ExecutionJob` with bounded size (configurable) to back pressure clients.

## State Transitions

- `RuntimeSessionState` transitions `Cold -> Warm -> Running -> Cancelling -> Draining -> Idle` per execution.
- `ExecutionJob` lifecycle `Enqueued -> Running -> Completed | Cancelled | Failed`.
- Resource guard states `Healthy -> Warning -> LimitExceeded` driving diagnostics.

## Data Volume & Scale Assumptions

- Single active job; queue length small (<=10) during backlog.
- Module artifacts stored under `~/.cache/hsjupyter/<hash>` with per-session temp directories.
- Streams limited to <=1 MB per cell by guard.
