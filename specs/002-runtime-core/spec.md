# Feature Specification: Phase 2 – Runtime Core

**Feature Branch**: `002-runtime-core`  
**Created**: 2025-10-24  
**Status**: Draft  
**Input**: User description: "Phase 2 – Runtime Core"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Stateful Execution Pipeline (Priority: P1)

As a notebook author I can execute Haskell cells sequentially and reuse prior definitions so that the kernel behaves like a real REPL-backed session.

**Why this priority**: Without persistent state the kernel cannot deliver meaningful notebook workflows, blocking downstream milestones.

**Independent Test**: Run a golden notebook where cell B references definitions from cell A; verify `execute_reply` returns the expected value and execution counts increment monotonically in the same session.

**Acceptance Scenarios**:

1. **Given** a fresh runtime session, **When** cell `let x = 2` executes, **Then** the runtime stores `x` and reports `execute_reply` status `ok` with execution count `1`.
2. **Given** the prior cell succeeded, **When** the author executes `x * 5`, **Then** the runtime reuses cached state, emits a stream/result of `10`, and marks execution count `2`.

---

### User Story 2 - Responsive Cancellation (Priority: P2)

As a maintainer I can interrupt a long-running cell and observe a deterministic `abort` outcome so runaway evaluations do not block subsequent work.

**Why this priority**: Controlled cancellation is critical for CI smoke tests and protects shared infrastructure from unresponsive code.

**Independent Test**: Launch a cell containing `threadDelay` or an infinite loop, issue `interrupt_request`, and confirm the runtime stops evaluation and surfaces `status=abort` within one second.

**Acceptance Scenarios**:

1. **Given** a cell executing, **When** the control channel sends `interrupt_request`, **Then** the runtime cancels worker threads, cleans up temporary artifacts, and replies with `status=abort`.

---

### User Story 3 - Failure Diagnostics & Resource Guards (Priority: P3)

As an on-call maintainer I need structured diagnostics and resource caps so I can triage compilation/runtime failures without attaching a debugger.

**Why this priority**: Rich error data and limits reduce MTTR and align with observability goals set in Phase 1.

**Independent Test**: Execute a cell that triggers a compilation error and another that allocates above the configured memory limit; verify structured diagnostics and throttling metrics appear in logs and telemetry.

**Acceptance Scenarios**:

1. **Given** a cell that fails to compile, **When** the runtime returns, **Then** the kernel emits diagnostics including module name, line numbers, and a user-friendly summary.
2. **Given** a cell exceeding the configured memory budget, **When** the guard trips, **Then** the runtime aborts execution, reports a `resource-limit` error, and keeps the session healthy.

---

### Edge Cases

- Connection loss or session reset while a cell executes; runtime must teardown safely and surface a restart-required error.
- Concurrent execute requests arriving before previous jobs finish; router must queue or reject additional work deterministically.
- Notebook sends code that mutates `GHC` flags or produces partial results; runtime should isolate options per session and flush buffered output before returning.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The runtime MUST maintain a persistent GHC session per notebook, incrementally loading cells and preserving state across executions.
- **FR-002**: The execution engine MUST stream stdout/stderr and rich payloads back to `JupyterBridge` while retaining execution ordering.
- **FR-003**: Runtime jobs MUST respect cancellation signals and propagate `abort` outcomes within 1 second of receiving an interrupt.
- **FR-004**: The system MUST surface structured diagnostics (module, span, severity, suggestion) for compilation and runtime exceptions.
- **FR-005**: The runtime MUST enforce configurable CPU/memory timeouts and clean up temporary artifacts after each cell.
- **FR-006**: Telemetry MUST record execution duration, queue depth, and resource-limit breaches for Phase 1 observability dashboards.

### Key Entities *(include if feature involves data)*

- **RuntimeSessionState**: Tracks loaded modules, imports, and evaluation context for a notebook session.
- **ExecutionJob**: Represents a queued cell with source code, metadata, cancellation token, and execution count.
- **ExecutionOutcome**: Structured result containing status (`ok`, `error`, `abort`), streams, diagnostics, and updated counters.
- **ResourceBudget**: Configuration for CPU time, memory ceiling, and evaluation timeout per session/job.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 95% of cells in the golden notebook execute successfully within 2 seconds while preserving state across cells.
- **SC-002**: Cancellation test completes with `status=abort` response in under 1 second from the interrupt being issued.
- **SC-003**: Compilation/runtime failures include diagnostics covering module, line, and severity for 100% of error cases exercised by the test suite.
- **SC-004**: Resource guard trials terminate and log `resource-limit` diagnostics without crashing the kernel for at least three consecutive soak runs.

## Assumptions

- Phase 2 will continue using the Phase 1 ZeroMQ bridge without introducing additional transport changes.
- The runtime relies on `ghcup`-managed GHC 9.6.x and can shell out to `cabal` only during session bootstrap, not per execution.
- External package installation and multi-session persistence remain out of scope until Phase 3 capability work.
