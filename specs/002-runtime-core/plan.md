# Implementation Plan: Phase 2 – Runtime Core

**Branch**: `002-runtime-core` | **Date**: 2025-10-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/002-runtime-core/spec.md`

## Summary

Design a resilient runtime subsystem that maintains a persistent GHC session per notebook, queues cell executions with cancellation support, and surfaces diagnostics/metrics consistent with Phase 1 observability gates.

## Technical Context

**Language/Version**: Haskell (GHC 9.6.x via ghcup)  
**Primary Dependencies**: GHC API / `hint` for evaluation, `async`, `stm`, `exceptions`, `temporary`, `aeson`  
**Storage**: In-memory session state; no external persistence in this phase  
**Testing**: `hspec` unit tests plus pyzmq/nbclient-driven integration notebooks  
**Target Platform**: Linux/macOS developer environments and CI containers  
**Project Type**: Backend service (HsJupyter kernel executable + supporting library modules)  
**Performance Goals**: <2s execution for golden notebook cells; cancellation latency ≤1s; heartbeat remains healthy during 30-minute soak  
**Constraints**: Enforce per-session CPU/memory timeouts; clean temporary artifacts; no network access during cell evaluation  
**Scale/Scope**: Single notebook session per kernel process; sequential job processing (parallelism deferred)

## Constitution Check

- Documentation-first: Spec + plan authored before implementation ✅  
- Test-first: Golden notebooks, cancellation, and diagnostics tests enumerated ✅  
- Observability: Metrics/logging requirements captured in spec ✅

## Project Structure

### Documentation (this feature)

```text
specs/002-runtime-core/
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── contracts/
└── spec.md
```

### Source Code (repository root)

```text
app/
└── KernelMain.hs            # entrypoint (wire runtime manager)

src/HsJupyter/
├── KernelProcess.hs         # orchestrates runtime lifecycle
├── Kernel/Types.hs          # shared config/state for runtime + bridge
├── Runtime/
│   ├── Manager.hs           # job queue, session orchestration
│   ├── Evaluation.hs        # GHC API / hint evaluation helpers
│   ├── SessionState.hs      # module/import cache, dependency graph
│   ├── ResourceGuard.hs     # CPU/memory/time budget enforcement
│   ├── Diagnostics.hs       # structured error translation
│   └── Telemetry.hs         # runtime metrics + logging glue
├── Router/RequestRouter.hs  # routes execute/control to Runtime.Manager
└── Bridge/…                 # existing Phase 1 bridge modules

test/unit/
├── RuntimeManagerSpec.hs
├── SessionStateSpec.hs
├── ResourceGuardSpec.hs
├── DiagnosticsSpec.hs
└── TelemetrySpec.hs

test/integration/
└── RuntimeNotebookSpec.hs   # golden notebook + cancellation scenarios
```

**Structure Decision**: Extend existing HsJupyter library with a `Runtime` namespace and targeted test suites; no additional projects required.

## Complexity Tracking

No constitution violations identified; additional complexity log not required.

## Phase 0 – Research Focus

- Evaluate GHC API vs `hint` trade-offs for incremental evaluation and sandboxing.  
- Prototype resource guard strategies (RTS options, process isolation, memory limits).  
- Catalogue failure modes (compilation errors, runtime exceptions, async exceptions) and map to diagnostic schema.

## Phase 1 – Design Deliverables

- `research.md`: captured decisions on tooling, cancellation mechanics, and resource enforcement.  
- `data-model.md`: detailed `RuntimeSessionState`, `ExecutionJob`, `ExecutionOutcome`, and `ResourceBudget`.  
- `contracts/`: message/result schema between Runtime Manager and Router for ok/error/abort states.  
- `quickstart.md`: instructions for running golden notebooks and cancellation demos.  
- Update agent context with runtime dependencies and testing strategy.

## Phase 2 – Task Generation Ready

Once design artifacts are complete, run `/speckit.tasks` to produce an execution plan covering:
- Runtime session management, evaluation, cancellation, and diagnostics modules.  
- Resource guard integration.  
- Unit and integration tests (golden notebook, cancellation, error reporting).  
- Documentation updates for quickstart and observability guidance.

## Constitution Re-Check

Design maintains documentation-first, test-first, and observability gates; proceed to `/speckit.tasks` after Phase 1 outputs exist.
