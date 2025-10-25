# Implementation Tasks: Installation & CLI Infrastructure

**Feature**: 004-install-cli  
**Branch**: `004-install-cli`  
**Created**: 2025-01-28  
**Spec**: [spec.md](./spec.md) | [Plan](./plan.md) | [Data Model](./data-model.md)

## Task Overview

**Total Tasks**: 32  
**Phases**: 6 (Setup + Foundational + 4 User Stories)  
**Parallel Opportunities**: 18 parallelizable tasks  
**MVP Scope**: User Story 1 (P1) - Core installation functionality

## User Story Completion Order

```text
Phase 1: Setup (project initialization)
Phase 2: Foundational (blocking prerequisites)
Phase 3: User Story 1 - P1 (Jupyter Kernel Installation) ← MVP
Phase 4: User Story 2 - P2 (Installation Diagnostics)  
Phase 5: User Story 3 - P3 (Custom Configuration)
Phase 6: User Story 4 - P3 (System Integration)
Phase 7: Polish & Cross-Cutting Concerns
```

## Independent Test Criteria

Each user story must be independently testable:

- **US1**: `hs-jupyter-kernel install` successfully registers kernel with Jupyter
- **US2**: `hs-jupyter-kernel doctor` identifies issues and provides solutions
- **US3**: Custom installation paths and configurations work correctly
- **US4**: JSON output and programmatic interfaces function as specified

---

## Phase 1: Setup & Project Initialization

**Goal**: Establish project structure and dependencies for CLI extension

- [x] T001 Add optparse-applicative dependency to hs-jupyter-kernel.cabal
- [x] T002 [P] Create CLI module directory structure under src/HsJupyter/CLI/
- [x] T003 [P] Create CLI test directory structure under test/unit/ and test/integration/
- [x] T004 [P] Update app/KernelMain.hs to support CLI subcommands while preserving existing behavior
- [x] T005 [P] Create basic project documentation in docs/cli/ for CLI architecture decisions

---

## Phase 2: Foundational Prerequisites

**Goal**: Core shared infrastructure that blocks all user stories

- [x] T006 Implement CLIDiagnostic error type extending RuntimeDiagnostic in src/HsJupyter/CLI/Types.hs
- [x] T007 Implement core data models (JupyterEnvironment, KernelInstallation) in src/HsJupyter/CLI/Types.hs
- [x] T008 [P] Implement system detection utilities in src/HsJupyter/CLI/Utilities.hs
- [x] T009 [P] Implement configuration management in src/HsJupyter/CLI/Configuration.hs
- [x] T010 Create unit tests for core data models in test/unit/CLITypesSpec.hs
- [x] T011 [P] Create unit tests for system detection in test/unit/SystemIntegrationSpec.hs

---

## Phase 3: User Story 1 - Jupyter Kernel Installation (P1)

**Goal**: Users can install HsJupyter kernel with `hs-jupyter-kernel install`

**Independent Test**: Installation command registers kernel and appears in `jupyter kernelspec list`

### Command Infrastructure

- [x] T012 [US1] Implement basic CLI command parser structure in src/HsJupyter/CLI/Commands.hs
- [x] T013 [US1] Implement InstallOptions data type and parser in src/HsJupyter/CLI/Commands.hs  
- [x] T014 [P] [US1] Create unit tests for command parsing in test/unit/CLICommandsSpec.hs

### Core Installation Logic

- [x] T015 [US1] Implement Jupyter environment detection in src/HsJupyter/CLI/Install.hs
- [x] T016 [US1] Implement kernelspec directory discovery and validation in src/HsJupyter/CLI/Install.hs
- [x] T017 [US1] Implement kernel.json generation with constitutional compliance in src/HsJupyter/CLI/Install.hs
- [x] T018 [US1] Implement kernel registration and file system operations in src/HsJupyter/CLI/Install.hs

### Constitutional Integration

- [x] T019 [US1] Integrate installation operations with ResourceGuard patterns in src/HsJupyter/CLI/Install.hs
- [ ] T020 [US1] Implement structured logging for installation operations via katip in src/HsJupyter/CLI/Install.hs
- [ ] T021 [US1] Implement cancellation support using TMVar patterns in src/HsJupyter/CLI/Install.hs

### US1 Testing & Validation

- [ ] T022 [P] [US1] Create unit tests for installation logic in test/unit/InstallSpec.hs
- [ ] T023 [US1] Create integration tests for end-to-end installation workflow in test/integration/CLIIntegrationSpec.hs
- [ ] T024 [US1] Implement basic kernel functionality verification after installation in src/HsJupyter/CLI/Install.hs

---

## Phase 4: User Story 2 - Installation Diagnostics (P2)

**Goal**: Users can diagnose installation issues with `hs-jupyter-kernel doctor`

**Independent Test**: Doctor command identifies system issues and provides actionable recommendations

### Diagnostic Infrastructure

- [ ] T025 [US2] Implement DiagnosticResult data model and analysis logic in src/HsJupyter/CLI/Doctor.hs
- [ ] T026 [P] [US2] Implement system health checking (Jupyter, GHC, kernel status) in src/HsJupyter/CLI/Doctor.hs
- [ ] T027 [P] [US2] Implement issue identification and severity classification in src/HsJupyter/CLI/Doctor.hs
- [ ] T028 [P] [US2] Implement recommendation generation with actionable solutions in src/HsJupyter/CLI/Doctor.hs

### US2 Testing & Integration

- [ ] T029 [P] [US2] Create unit tests for diagnostic functionality in test/unit/DoctorSpec.hs
- [ ] T030 [US2] Create integration tests for diagnostic workflows in test/integration/CLIIntegrationSpec.hs

---

## Phase 5: User Story 3 - Custom Installation Configuration (P3)

**Goal**: Advanced users can customize installation paths and kernel configuration

**Independent Test**: Custom paths and configuration options work as specified

### Configuration Extensions

- [ ] T031 [US3] Extend InstallOptions with custom path and configuration support in src/HsJupyter/CLI/Commands.hs
- [ ] T032 [P] [US3] Implement custom path validation and resolution in src/HsJupyter/CLI/Config.hs
- [ ] T033 [P] [US3] Extend kernel.json generation with custom configuration options in src/HsJupyter/CLI/Install.hs

### US3 Testing

- [ ] T034 [P] [US3] Create integration tests for custom configuration scenarios in test/integration/CLIIntegrationSpec.hs

---

## Phase 6: User Story 4 - System Integration and Verification (P3)

**Goal**: Programmatic access through JSON output and silent operation modes

**Independent Test**: JSON output format matches specifications and automation scenarios work

### Programmatic Interface

- [ ] T035 [US4] Implement JSON output formatting for all commands in src/HsJupyter/CLI/Output.hs
- [ ] T036 [P] [US4] Implement quiet mode and non-interactive operation in src/HsJupyter/CLI/Commands.hs
- [ ] T037 [P] [US4] Implement list and version commands for system integration in src/HsJupyter/CLI/Commands.hs
- [ ] T038 [P] [US4] Implement uninstall command with cleanup verification in src/HsJupyter/CLI/Commands.hs

### US4 Testing & Validation

- [ ] T039 [P] [US4] Create integration tests for JSON output and automation scenarios in test/integration/CLIIntegrationSpec.hs
- [ ] T040 [US4] Validate JSON schema compliance with contracts/json-schema.md in test/integration/CLIIntegrationSpec.hs

---

## Phase 7: Polish & Cross-Cutting Concerns

**Goal**: Performance optimization, documentation, and production readiness

- [ ] T041 [P] Performance testing and optimization to meet constitutional targets (<2min install, <5s diagnostics)
- [ ] T042 [P] Cross-platform testing on Linux, macOS, Windows environments
- [ ] T043 [P] Documentation updates including CLI usage examples and troubleshooting guides
- [ ] T044 Integration with existing constitutional audit and compliance verification

---

## Parallel Execution Examples

### Setup Phase (4 parallel tasks)

```bash
# These can run simultaneously:
T002, T003, T004, T005
```

### User Story 1 Implementation (7 parallel tasks)

```bash
# After T012-T013 complete, these can run in parallel:
T014, T015, T016, T017, T022
# After core logic complete:
T019, T020, T021 (constitutional integration)
```

### User Story 2 Implementation (4 parallel tasks)

```bash
# After T025 complete:
T026, T027, T028, T029
```

## Implementation Strategy

### MVP Delivery (User Story 1 Only)

Focus on tasks T001-T024 for initial delivery:

- Basic `hs-jupyter-kernel install` functionality
- Constitutional compliance (error handling, logging, resource management)
- Unit and integration tests
- Documentation for installation workflow

### Incremental Delivery

Each user story builds incrementally:

1. **MVP**: Core installation (US1)
2. **V1.1**: Add diagnostics (US1 + US2)  
3. **V1.2**: Add customization (US1 + US2 + US3)
4. **V1.3**: Full programmatic access (US1 + US2 + US3 + US4)

### Risk Mitigation

**Medium Risk Items** (extra attention needed):

- T008: Cross-platform path handling for various Jupyter installations
- T016: Jupyter environment detection across Python installations (conda, pip, system)
- T018: Permission handling for user vs system installation modes
- T042: Cross-platform compatibility validation

**Constitutional Compliance Tasks** (must pass constitutional gates):

- T006: Error handling integration
- T019-T021: ResourceGuard, logging, cancellation patterns
- T041: Performance targets validation

## Task Dependencies

```text
Setup → Foundational → User Stories (P1 → P2 → P3 → P3) → Polish

Critical Path:
T001 → T006,T007 → T012,T013 → T015-T018 → T023 (MVP complete)

Parallel Streams:
- Testing: T010,T011,T014,T022,T029,T034,T039 (can run alongside implementation)
- Documentation: T005,T043 (can run independently)
- Constitutional: T019-T021 (after core logic)
```

## Success Metrics

- **Task Completion**: All 44 tasks with constitutional compliance
- **Performance**: <2min install, <5s diagnostics, <100MB memory usage
- **Test Coverage**: 100% of acceptance scenarios automated
- **Platform Support**: Linux, macOS, Windows compatibility verified
- **User Experience**: 95% installation success rate on standard systems
