# Tasks: Jupyter Kernel Integration Bug Fix

**Input**: Design documents from `/specs/005-fix-jupyter-kernel-bug/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Per the constitution, every user story must include the tests that will fail before implementation (unit, integration, docs as appropriate). Document these explicitly.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., User Story 1, User Story 2, User Story 3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Prepare the development environment.

- [X] T001 Verify that all dependencies are correctly listed in `hs-jupyter-kernel.cabal`.
- [X] T002 [P] Configure a test runner in `cabal.project` to easily execute `hspec` tests.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Implement the core protocol and lifecycle management.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [X] T003 Implement the ZMQ socket handling and message (de)serialization in `src/HsJupyter/Bridge/Protocol/Codec.hs`.
- [X] T004 Implement the main kernel loop and lifecycle management (startup, shutdown, heartbeat) in `src/HsJupyter/KernelProcess.hs`.
- [X] T005 Implement the request router for dispatching messages to the correct handlers in `src/HsJupyter/Router.hs`.
- [X] T006 Implement a basic `kernel_info_request` handler to provide kernel metadata.

**Checkpoint**: Foundation ready - The kernel can start, respond to info requests, and shut down cleanly.

---

## Phase 3: User Story 1 - Execute Code and Receive Output (Priority: P1) 🎯 MVP

**Goal**: A user can execute a simple Haskell expression and see the correct output in their Jupyter notebook.

**Independent Test**: Run a Jupyter notebook, execute a cell with `1 + 1`, and verify that the output `2` is displayed.

### Tests for User Story 1 ⚠️

> **MANDATORY: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T007 [P] [User Story 1] Create an integration test in `test/integration/ExecuteEchoSpec.hs` (stubbed - needs rewrite for current implementation)

### Implementation for User Story 1

- [X] T008 [User Story 1] Implement the `execute_request` handler in the `RequestRouter` to forward code to the runtime.
- [X] T009 [User Story 1] Implement the GHC runtime evaluation logic in `src/HsJupyter/Runtime/Evaluation.hs` to execute the code.
- [X] T010 [User Story 1] Ensure that `stdout` and `stderr` are captured and sent back as `stream` messages.
- [X] T011 [User Story 1] Ensure the final result is sent back in an `execute_result` message.
- [X] T012 [User Story 1] Add structured logging in the execution path for easier debugging.

**Checkpoint**: User Story 1 implementation complete. Unit tests pass (248 examples, 0 failures, 11 pending). Integration test needs rewrite.

---

## Phase 4: User Story 2 - Handle Kernel Interruptions and Restarts (Priority: P2)

**Goal**: A user can interrupt a long-running computation and restart the kernel without crashing Jupyter.

**Independent Test**: Execute a cell with an infinite loop, interrupt it using the Jupyter UI, and then restart the kernel. The kernel should become responsive again.

### Tests for User Story 2 ⚠️

- [X] T013 [P] [User Story 2] Create an integration test that simulates an `interrupt_request` during a long-running execution.

### Implementation for User Story 2

- [X] T014 [User Story 2] Implement the `interrupt_request` handler in `src/HsJupyter/Router.hs`.
- [X] T015 [User Story 2] Use STM or async exceptions to gracefully terminate the running computation in the GHC runtime.
- [X] T016 [User Story 2] Implement the `shutdown_request` handler to support kernel restarts.

**Checkpoint**: User Stories 1 AND 2 implementation complete. Integration tests need validation with real kernel.

---

## Phase N: Polish & Cross-Cutting Concerns

**Purpose**: Final improvements and validation.

- [X] T017 [P] Update `README.md` with any changes to the installation or usage instructions.
- [X] T018 Run the `quickstart.md` validation steps to ensure the end-to-end workflow is correct.
- [X] T019 Code cleanup and refactoring in the modified modules.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies.
- **Foundational (Phase 2)**: Depends on Setup.
- **User Stories (Phase 3+)**: Depend on Foundational.
- **Polish (Final Phase)**: Depends on all user stories.

### User Story Dependencies

- **User Story 1 (P1)**: Depends on Foundational.
- **User Story 2 (P2)**: Depends on Foundational and User Story 1.

### Within Each User Story

- Tests MUST be written and FAIL before implementation.
- Core protocol logic before specific handlers.

### Parallel Opportunities

- T002 can be done in parallel with other setup tasks.
- T007 and T013 (tests) can be written in parallel with the foundational work.

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test User Story 1 independently.

### Incremental Delivery

1. Complete Setup + Foundational.
2. Add User Story 1 → Test independently.
3. Add User Story 2 → Test independently.

---

## Constitution Guidance

Follow HsJupyter Constitution v1.2.0:

### Core Principles

- **Simplicity & Maintainability (VI)**: Apply DRY, KISS, and YAGNI.
- **Modular Architecture & Strong Design (V)**: Apply SOLID, composition over inheritance, separation of concerns.
- **Resilience & Defensive Programming (VII)**: Handle failures gracefully, validate inputs, use structured error types.
- **Pragmatic Balance (VIII)**: Apply the Rule of Three, balance cohesion and coupling.
