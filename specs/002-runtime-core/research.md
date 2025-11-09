# Research Summary: Phase 2 – Runtime Core

## Decision 1: GHC API vs hint

- **Decision**: Use the GHC API directly (via `ghc-lib` interfaces) for session management while wrapping evaluation helpers similar to `hint`.
- **Rationale**: Offers fine-grained control over session state, module loading, and diagnostics without the performance penalty/black-box behaviour of `hint`.
- **Alternatives considered**: Pure `hint` (simpler API but less control over concurrency/cancellation), external GHC process per cell (strong isolation but slower and complicates state reuse).

## Decision 2: Execution Job Architecture

- **Decision**: Introduce a single-threaded job queue (`TBQueue ExecutionJob`) managed by `Runtime.Manager`, ensuring ordered execution with cancellation hooks.
- **Rationale**: Matches Jupyter semantics (one execution at a time), simplifies state management, and provides a clear interception point for cancellation/resource guards.
- **Alternatives considered**: Fully parallel execution (increases complexity, potential state races), ad-hoc IORef-based state (less robust under cancellation).

## Decision 3: Resource Guard Strategy

- **Decision**: Combine RTS options (time/memory limits) with per-job timers and a lightweight watchdog thread to enforce CPU timeouts.
- **Rationale**: Keeps enforcement in-process without introducing separate sandboxes yet still provides deterministic limits aligned with Phase 1 goals.
- **Alternatives considered**: OS-level sandbox (higher security but significant engineering), post-exec log parsing (non-real-time feedback).

## Decision 4: Diagnostics Format

- **Decision**: Use a structured diagnostic record (`RuntimeDiagnostics`) capturing severity, module, span, summary, and suggestions, encoded to JSON for the bridge.
- **Rationale**: Aligns with Phase 1 success criteria and enables frontend display/telemetry.
- **Alternatives considered**: Plain text messages (simpler but harder to parse), streaming GHC raw output (noisy, inconsistent).

## Decision 5: Testing Approach

- **Decision**: Golden notebook executed via `nbclient`/pyzmq harness for integration, supplemented by `HSPEC` unit tests for session state, cancellation, resource guard, and diagnostics.
- **Rationale**: Reuses Phase 1 demo tooling, ensures coverage of sequential execution, and isolates core components for unit testing.
- **Alternatives considered**: Custom Python harness only (less coverage), manual testing (violates test-first gate).
