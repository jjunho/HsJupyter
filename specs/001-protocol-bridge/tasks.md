# Tasks: Phase 1 – Protocol Bridge

**Input**: Design documents from `/specs/001-protocol-bridge/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/, quickstart.md

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Establish the Haskell project scaffolding and tooling required by all stories.

- [X] T001 Create executable/library skeleton in `hs-jupyter-kernel.cabal` matching modules from the implementation plan.
- [X] T002 Define workspace tooling in `cabal.project` and enable `test/` + `scripts/demo` directories for builds.
- [X] T003 [P] Add base directory structure (`app/`, `src/HsJupyter/`, `test/`, `scripts/demo/`) with placeholder modules so cabal builds succeed.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core protocol and configuration components that all user stories rely on.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [X] T004 Implement `KernelProcessConfig` parsing and validation in `src/HsJupyter/KernelProcess.hs` using `aeson` and CLI overrides.
- [X] T005 [P] Define protocol data structures in `src/HsJupyter/Bridge/Protocol/Envelope.hs` and supporting enums/types.
- [X] T006 [P] Implement JSON codec utilities and HMAC helpers in `src/HsJupyter/Bridge/Protocol/Codec.hs` covering signature calculation and verification.
- [X] T007 Author unit tests for protocol codec round-trips in `test/unit/ProtocolEnvelopeSpec.hs` to lock in message framing.

**Checkpoint**: Foundation ready – user story implementation can now begin in parallel.

---

## Phase 3: User Story 1 - Complete Execute Echo (Priority: P1) 🎯 MVP

**Goal**: Demonstrate end-to-end execute echo loop from nbclient through the kernel with control-channel interrupt handling.

**Independent Test**: Run `scripts/demo/phase1_echo_notebook.py --connection scripts/demo/sample-connection.json` and observe `status=ok` plus streamed stdout within 2 seconds.

### Tests for User Story 1 ⚠️ Write before implementation

- [X] T008 [P] [US1] Create nbclient-driven round-trip test in `test/integration/ExecuteEchoSpec.hs` covering echo success and interrupt ack.
- [X] T009 [P] [US1] Implement demo harness `scripts/demo/phase1_echo_notebook.py` to drive the golden notebook scenario.

### Implementation for User Story 1

- [X] T010 [US1] Implement stub runtime echo behaviour in `src/HsJupyter/Runtime/EchoRuntime.hs` emitting deterministic streams and execution counts.
- [X] T011 [US1] Implement request routing in `src/HsJupyter/Router/RequestRouter.hs` to forward execute/control frames to the runtime and build replies.
- [X] T012 [US1] Wire ZeroMQ sockets, signature verification hook, and reply emission in `src/HsJupyter/Bridge/JupyterBridge.hs` (happy-path only).
- [X] T013 [US1] Bootstrap kernel lifecycle in `app/KernelMain.hs`, invoking `KernelProcess` startup, bridge loop, and graceful shutdown.

**Checkpoint**: Execute echo path demonstrably round-trips and control interrupts return `ok` without crashing the kernel.

---

## Phase 4: User Story 2 - Enforced Protocol Guardrails (Priority: P2)

**Goal**: Reject unsigned or malformed envelopes while keeping valid traffic unaffected.

**Independent Test**: Replay a bad HMAC using the demo harness and confirm the kernel logs `invalid_signature` while subsequent valid messages succeed.

### Tests for User Story 2 ⚠️ Write before implementation

- [X] T014 [P] [US2] Add negative-path spec `test/unit/SignatureValidationSpec.hs` that feeds invalid HMACs and asserts drop + warning behaviour.

### Implementation for User Story 2

- [X] T015 [US2] Extend `src/HsJupyter/Bridge/JupyterBridge.hs` to enforce HMAC checks, emit structured warnings, and maintain socket health after drops.
- [X] T016 [US2] Add guardrail telemetry in `src/HsJupyter/KernelProcess.hs` (or dedicated metrics module) tracking rejected message counts.

**Checkpoint**: Malformed envelopes are detected, dropped, and logged without interrupting valid message flow.

---

## Phase 5: User Story 3 - Operability Insights (Priority: P3)

**Goal**: Provide baseline diagnostics (logs + heartbeat metrics) for on-call operators.

**Independent Test**: Launch kernel with `HSJUPYTER_LOG_LEVEL=debug` and confirm heartbeat latency + correlation IDs in logs while the demo runs.

### Tests for User Story 3 ⚠️ Write before implementation

- [X] T017 [P] [US3] Add logging/metrics spec `test/unit/ObservabilitySpec.hs` verifying structured log fields and heartbeat state transitions.

### Implementation for User Story 3

- [X] T018 [US3] Instrument correlation IDs and channel tagging in `src/HsJupyter/Bridge/JupyterBridge.hs` log output.
- [X] T019 [US3] Implement heartbeat responder thread and latency tracking in `src/HsJupyter/Bridge/HeartbeatThread.hs` with integration into bridge supervision.
- [X] T020 [US3] Surface metrics/log configuration options in `app/KernelMain.hs` and document environment toggles in `docs/developer/README.md`.

**Checkpoint**: Debug logging provides actionable metadata and heartbeat monitoring meets success criteria.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, packaging, and quality pass across all user stories.

- [X] T021 [P] Update `quickstart.md` and `docs/architecture.md` to reflect implemented ZeroMQ flow and observability knobs.
- [X] T022 Consolidate sample connection files and demo assets in `scripts/demo/` with README updates.
- [X] T023 Run full `cabal v2-test` and record results in `specs/001-protocol-bridge/quickstart.md` troubleshooting section.
- [X] T024 [P] Prepare release notes snippet in `docs/roadmap.md` marking Phase 1 tasks complete.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)** → prerequisite for all subsequent work.
- **Phase 2 (Foundational)** → depends on Phase 1; blocks all user stories until complete.
- **Phase 3 (US1)** → depends on Phase 2 completion; unlocks MVP demo.
- **Phase 4 (US2)** → depends on Phase 2; may run after or alongside US1 once shared files stabilise.
- **Phase 5 (US3)** → depends on Phase 2; can begin after US1 stabilises to avoid log schema churn.
- **Phase 6 (Polish)** → depends on completion of targeted user stories (US1–US3).

### User Story Dependencies

- **US1** stands alone once foundational components exist.
- **US2** builds on US1’s bridge implementation but must keep guardrails isolated to avoid regressions.
- **US3** extends logging/heartbeat around the same bridge; coordinate sequencing to prevent merge conflicts.

### Within-Story Ordering

- Write and fail tests (T008, T009, T014, T017) before implementing behaviour.
- Implement runtime/bridge logic before wiring CLI bootstrap.
- Update documentation only after behaviour is validated.

### Parallel Opportunities

- Setup task T003 can run alongside cabal file authoring once filenames are final.
- Foundational tasks T005 and T006 can progress in parallel; both depend on T004 for config types.
- Within US1, T008 and T009 can progress concurrently while implementation waits for test scaffolds.
- US2’s telemetry task (T016) can progress once T015 establishes rejection hooks.
- US3 tasks T018–T020 should run sequentially to avoid logging conflicts, but T017 can be authored in parallel.
- Polish tasks T021 and T024 can proceed in parallel after core stories complete.

---

## Parallel Example: User Story 1

```bash
# Parallel test scaffolding before implementation
- T008 [P] [US1] Create nbclient-driven round-trip test in test/integration/ExecuteEchoSpec.hs
- T009 [P] [US1] Implement demo harness scripts/demo/phase1_echo_notebook.py

# After tests exist, parallelise runtime and router work once interfaces agreed
- T010 [US1] Implement stub runtime echo behaviour in src/HsJupyter/Runtime/EchoRuntime.hs
- T011 [US1] Implement request routing in src/HsJupyter/Router/RequestRouter.hs
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1 + Phase 2 to establish buildable project foundation.
2. Execute Phase 3 tasks (T008–T013) to deliver the execute echo demo.
3. Validate nbclient round-trip and control interrupt handling before expanding scope.

### Incremental Delivery

1. Deliver US1 (echo loop) as the Phase 1 milestone.
2. Layer US2 guardrails to harden the bridge without affecting US1 tests.
3. Add US3 observability enhancements to support operators and CI diagnostics.

### Parallel Team Strategy

- Developer A: Owns foundational protocol types (T004–T007) and US1 runtime/bridge work (T010–T013).
- Developer B: Focuses on guardrail validation and telemetry (T014–T016).
- Developer C: Drives observability enhancements and heartbeat metrics (T017–T020) plus polish documentation (T021–T024).

---

## Notes

- Maintain sequential task IDs (T001–T024) when updating progress.
- Mark completed tasks with `[X]` during implementation as required by `/speckit.implement`.
- Tests are front-loaded per story to preserve the constitution’s test-first gate.
- Re-run nbclient demo after each story to ensure independence and avoid regressions.
