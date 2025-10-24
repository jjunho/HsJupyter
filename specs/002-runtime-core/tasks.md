# Tasks: Phase 2 – Runtime Core

**Input**: Design documents from `/specs/002-runtime-core/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Parallelizable when files/concerns do not overlap
- **[Story]**: User story label (US1, US2, US3)
- All tasks include concrete file paths

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Prepare build/test tooling for runtime additions.

- [X] T001 Update `hs-jupyter-kernel.cabal` with runtime modules and dependencies (`ghc`, `hint`, `exceptions`, `stm`, `temporary`).
- [X] T002 Ensure `cabal.project` and `.gitignore` include runtime artifacts (e.g., `.ghci-tmp`, `dist-newstyle` already covered) and add sandbox temp directories if needed.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and scaffolding all stories depend on.

- [X] T003 Create runtime types in `src/HsJupyter/Runtime/SessionState.hs` (`RuntimeSessionState`, `ExecutionJob`, `ExecutionOutcome`, `ResourceBudget`).
- [X] T004 [P] Implement diagnostics schema in `src/HsJupyter/Runtime/Diagnostics.hs` with JSON helpers and severity definitions.
- [X] T005 [P] Add telemetry helpers in `src/HsJupyter/Runtime/Telemetry.hs` for metrics/logging hooks used across stories.
- [X] T006 Introduce runtime namespace exports in `HsJupyter/Kernel/Types.hs` and update plan docs accordingly.

**Checkpoint**: Session state, diagnostics, telemetry scaffolds exist; runtime manager can compile against new types.

---

## Phase 3: User Story 1 – Stateful Execution Pipeline (Priority: P1) 🎯 MVP

**Goal**: Persistent GHC session with sequential execution and reusable state.

**Independent Test**: Golden notebook where cell B reuses cell A definitions passes via pyzmq harness.

### Tests for User Story 1

- [ ] T007 [P] [US1] Add unit tests in `test/unit/SessionStateSpec.hs` for loading modules, binding updates, execution count increments.
- [ ] T008 [US1] Add integration golden notebook test `test/integration/RuntimeNotebookSpec.hs` verifying sequential state reuse.

### Implementation for User Story 1

- [ ] T009 [US1] Implement `Runtime.Manager` job queue and session lifecycle in `src/HsJupyter/Runtime/Manager.hs`.
- [ ] T010 [US1] Implement evaluation helpers using GHC API in `src/HsJupyter/Runtime/Evaluation.hs` (load, compile, run).
- [ ] T011 [US1] Wire `KernelProcess.hs` and `RequestRouter.hs` to use `Runtime.Manager` for execute requests, returning updated outcomes.

**Checkpoint**: Sequential cells run with persistent state; integration test verifies behaviour.

---

## Phase 4: User Story 2 – Responsive Cancellation (Priority: P2)

**Goal**: Interrupt long-running cells, returning `status=abort` promptly.

### Tests for User Story 2

- [ ] T012 [P] [US2] Extend `RuntimeNotebookSpec.hs` with cancellation scenario exercising `interrupt_request`.
- [ ] T013 [US2] Add unit test in `test/unit/RuntimeManagerSpec.hs` covering cancellation token propagation.

### Implementation for User Story 2

- [ ] T014 [US2] Add cancel tokens and async management to `Runtime.Manager` (register jobs, cancel evaluation threads).
- [ ] T015 [US2] Ensure `KernelProcess` control loop forwards interrupts to runtime manager and releases queued jobs cleanly.

**Checkpoint**: Cancellation test passes; runtime responds with `status=abort` within SLA.

---

## Phase 5: User Story 3 – Diagnostics & Resource Guards (Priority: P3)

**Goal**: Structured diagnostics and resource enforcement for failures.

### Tests for User Story 3

- [ ] T016 [P] [US3] Add unit tests in `test/unit/DiagnosticsSpec.hs` for compilation/runtime error translation.
- [ ] T017 [P] [US3] Add unit tests in `test/unit/ResourceGuardSpec.hs` for CPU/memory limits and stream truncation.
- [ ] T018 [US3] Extend integration test to cover resource limit breach logging.

### Implementation for User Story 3

- [ ] T019 [US3] Implement `ResourceGuard` watchdog (timers, RTS options, memory tracking) in `src/HsJupyter/Runtime/ResourceGuard.hs`.
- [ ] T020 [US3] Integrate diagnostics emission in `Runtime.Manager`/`Evaluation` (structured error handling, telemetry).
- [ ] T021 [US3] Emit telemetry metrics/logs using `Runtime/Telemetry.hs` and ensure `KernelProcess` forwards metrics to bridge/logging stack.

**Checkpoint**: Error and resource guard tests pass; telemetry logs are produced for failures.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final documentation, demos, and refactor touches.

- [ ] T022 Update `specs/002-runtime-core/quickstart.md` with runtime demo steps and golden notebook instructions.
- [ ] T023 Add developer docs in `docs/developer/README.md` covering runtime queue usage, cancellation flags, resource tuning.
- [ ] T024 Run full `cabal v2-test` and record results/known issues in quickstart troubleshooting.
- [ ] T025 [P] Refine logging format (correlation IDs, resource guard events) in `KernelProcess.hs` and `Runtime/Telemetry.hs`.

---

## Dependencies & Execution Order

- Setup (Phase 1) precedes foundational work.
- Foundational (Phase 2) must complete before user story tasks begin.
- User stories follow priority order: US1 (stateful execution) → US2 (cancellation) → US3 (diagnostics/guards). Later stories depend on runtime manager from US1.
- Tests precede implementation tasks within each story.
- Polish phase executes after all stories meet acceptance.

## Parallel Opportunities

- T003–T005 (session state, diagnostics, telemetry) can progress in parallel once interfaces agreed.
- Within US1, tests T007/T008 can run concurrently; implementations T009–T011 should follow queue/evaluation wiring order.
- US3 unit tests T016/T017 can be authored while resource guard implementation is in progress.
- Polish tasks T022–T025 can be split between contributors after core functionality stabilises.

## Implementation Strategy

1. Complete foundational runtime scaffolding (types, diagnostics, telemetry).
2. Deliver US1 sequential execution (MVP) with green tests.
3. Layer cancellation (US2) to satisfy interrupt SLA.
4. Add diagnostics and resource guards (US3) plus telemetry refinements.
5. Finish documentation/polish and rerun full test suite.
