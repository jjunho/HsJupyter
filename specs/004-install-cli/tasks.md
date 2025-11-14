# Tasks: Installation & CLI Infrastructure

**Input**: Design documents from `/specs/004-install-cli/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Per the constitution, every user story includes integration tests that will fail before implementation. Tests are executed before core implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- Single Haskell project structure
- Source: `src/HsJupyter/CLI/`
- Tests: `test/unit/` and `test/integration/`
- All paths relative to repository root: `/home/jjunho/projetos/10.outubro/HsJupyter`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic CLI structure

- [X] T001 Create CLI module directory structure at src/HsJupyter/CLI/
- [X] T002 Add optparse-applicative dependency to hs-jupyter-kernel.cabal
- [X] T003 [P] Create CLI.Types module with core data types in src/HsJupyter/CLI/Types.hs
- [X] T004 [P] Create CLI.Output module for JSON/text formatting in src/HsJupyter/CLI/Output.hs
- [X] T005 [P] Create CLI.Utilities module for shared helpers in src/HsJupyter/CLI/Utilities.hs

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T006 Implement InstallScope type and validation in src/HsJupyter/CLI/Types.hs
- [X] T007 Implement CustomPaths type and path resolution in src/HsJupyter/CLI/Types.hs
- [X] T008 Implement ValidationLevel type for installation validation in src/HsJupyter/CLI/Types.hs
- [X] T009 [P] Implement JupyterEnvironment detection logic in src/HsJupyter/CLI/Utilities.hs
- [X] T010 [P] Implement KernelInstallation status checking in src/HsJupyter/CLI/Utilities.hs
- [X] T011 [P] Create CLIDiagnostic error types extending RuntimeDiagnostic in src/HsJupyter/CLI/Types.hs
- [X] T012 Implement JSON output formatters in src/HsJupyter/CLI/Output.hs
- [X] T013 Implement human-readable output formatters in src/HsJupyter/CLI/Output.hs
- [X] T014 Create base CLI command parser structure using optparse-applicative in src/HsJupyter/CLI/Commands.hs
- [X] T015 Integrate CLI commands into main entry point in app/KernelMain.hs

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Jupyter Kernel Installation (Priority: P1) 🎯 MVP

**Goal**: Enable users to install HsJupyter kernel into existing Jupyter environment with simple `hs-jupyter-kernel install` command

**Independent Test**: Run `hs-jupyter-kernel install` on clean system with Jupyter installed and verify kernel appears in `jupyter kernelspec list` and launches successfully

### Integration Tests for User Story 1 ⚠️

> **MANDATORY: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T016 [P] [US1] Integration test for default installation workflow in test/integration/CLIIntegrationSpec.hs
- [X] T017 [P] [US1] Integration test for force reinstallation workflow in test/integration/CLIIntegrationSpec.hs
- [X] T018 [P] [US1] Integration test for user-scoped installation workflow in test/integration/CLIIntegrationSpec.hs

### Unit Tests for User Story 1 ⚠️

- [X] T019 [P] [US1] Unit tests for install command parsing in test/unit/CLICommandsSpec.hs
- [X] T020 [P] [US1] Unit tests for installation scope selection in test/unit/CLIInstallSpec.hs
- [X] T021 [P] [US1] Unit tests for kernelspec generation in test/unit/CLIInstallSpec.hs

### Implementation for User Story 1

- [X] T022 [US1] Create CLI.Install module with installation orchestration in src/HsJupyter/CLI/Install.hs
- [X] T023 [US1] Implement Jupyter environment detection (find kernelspec directories) in src/HsJupyter/CLI/Install.hs
- [X] T024 [US1] Implement GHC path detection and validation in src/HsJupyter/CLI/Install.hs
- [X] T025 [US1] Implement kernel.json template generation from kernelspec/hsjupyter/kernel.json.template
- [X] T026 [US1] Implement kernelspec directory creation and file writing in src/HsJupyter/CLI/Install.hs
- [X] T027 [US1] Implement installation scope resolution (user vs system) in src/HsJupyter/CLI/Install.hs
- [X] T028 [US1] Implement force reinstallation logic (detect existing and overwrite) in src/HsJupyter/CLI/Install.hs
- [X] T029 [US1] Implement basic installation validation (files exist and readable) in src/HsJupyter/CLI/Install.hs
- [X] T030 [US1] Add install command parser options to src/HsJupyter/CLI/Commands.hs
- [X] T031 [US1] Implement install command execution handler in src/HsJupyter/CLI/Commands.hs
- [X] T032 [US1] Add structured logging for installation operations using katip in src/HsJupyter/CLI/Install.hs
- [X] T033 [US1] Implement resource guards for installation timeout and disk space in src/HsJupyter/CLI/Install.hs
- [X] T034 [US1] Add installation success/failure output formatting in src/HsJupyter/CLI/Output.hs

**Checkpoint**: At this point, User Story 1 should be fully functional - users can install HsJupyter kernel with basic options

---

## Phase 4: User Story 2 - Installation Diagnostics and Troubleshooting (Priority: P2)

**Goal**: Provide clear diagnostic information and guided troubleshooting through `hs-jupyter-kernel doctor` command

**Independent Test**: Run `hs-jupyter-kernel doctor` on systems with various configuration issues and verify helpful diagnostic output with actionable recommendations

### Integration Tests for User Story 2 ⚠️

- [X] T035 [P] [US2] Integration test for doctor command on healthy system in test/integration/CLIIntegrationSpec.hs
- [X] T036 [P] [US2] Integration test for doctor command with missing dependencies in test/integration/CLIIntegrationSpec.hs
- [X] T037 [P] [US2] Integration test for doctor command with corrupted installation in test/integration/CLIIntegrationSpec.hs

### Unit Tests for User Story 2 ⚠️

- [X] T038 [P] [US2] Unit tests for diagnostic checks in test/unit/DoctorSpec.hs
- [X] T039 [P] [US2] Unit tests for issue severity classification in test/unit/DoctorSpec.hs
- [X] T040 [P] [US2] Unit tests for recommendation generation in test/unit/DoctorSpec.hs

### Implementation for User Story 2

- [X] T041 [US2] Create CLI.Doctor module with diagnostic orchestration in src/HsJupyter/CLI/Doctor.hs
- [X] T042 [P] [US2] Implement Jupyter installation check (jupyter command availability) in src/HsJupyter/CLI/Doctor.hs
- [X] T043 [P] [US2] Implement Jupyter version detection and parsing in src/HsJupyter/CLI/Doctor.hs
- [X] T044 [P] [US2] Implement kernelspec directory accessibility check in src/HsJupyter/CLI/Doctor.hs
- [X] T045 [P] [US2] Implement HsJupyter kernel installation status check in src/HsJupyter/CLI/Doctor.hs
- [X] T046 [P] [US2] Implement kernel functionality test (basic evaluation) in src/HsJupyter/CLI/Doctor.hs
- [X] T047 [P] [US2] Implement GHC availability and version check in src/HsJupyter/CLI/Doctor.hs
- [X] T048 [P] [US2] Implement system information collection (platform, arch, PATH) in src/HsJupyter/CLI/Doctor.hs
- [X] T049 [US2] Implement issue detection and severity classification in src/HsJupyter/CLI/Doctor.hs
- [X] T050 [US2] Implement recommendation generation based on detected issues in src/HsJupyter/CLI/Doctor.hs
- [X] T051 [US2] Implement overall health status determination in src/HsJupyter/CLI/Doctor.hs
- [X] T052 [US2] Add doctor command parser options to src/HsJupyter/CLI/Commands.hs
- [X] T053 [US2] Implement doctor command execution handler in src/HsJupyter/CLI/Commands.hs
- [X] T054 [US2] Add diagnostic result output formatting (JSON and human-readable) in src/HsJupyter/CLI/Output.hs
- [X] T055 [US2] Implement exit code mapping based on health status in src/HsJupyter/CLI/Doctor.hs

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently - users can install and diagnose issues

---

## Phase 5: User Story 3 - Custom Installation Configuration (Priority: P3)

**Goal**: Enable advanced users to customize installation paths, kernel configurations, and integration settings for specific deployment environments

**Independent Test**: Install with custom configuration options and verify kernel operates with specified settings (custom paths, resource limits, GHC version)

### Integration Tests for User Story 3 ⚠️

- [X] T056 [P] [US3] Integration test for custom path installation in test/integration/CLIIntegrationSpec.hs
- [X] T057 [P] [US3] Integration test for custom kernel configuration in test/integration/CLIIntegrationSpec.hs
- [X] T058 [P] [US3] Integration test for custom GHC path specification in test/integration/CLIIntegrationSpec.hs

### Unit Tests for User Story 3 ⚠️

- [X] T059 [P] [US3] Unit tests for custom path validation in test/unit/CLIInstallSpec.hs
- [X] T060 [P] [US3] Unit tests for kernel configuration merging in test/unit/CLIInstallSpec.hs
- [X] T061 [P] [US3] Unit tests for resource limit validation in test/unit/CLITypesSpec.hs

### Implementation for User Story 3

- [X] T062 [US3] Create CLI.Configuration module for configuration management in src/HsJupyter/CLI/Configuration.hs
- [X] T063 [P] [US3] Implement custom Jupyter directory path resolution in src/HsJupyter/CLI/Configuration.hs
- [X] T064 [P] [US3] Implement custom kernelspec directory path resolution in src/HsJupyter/CLI/Configuration.hs
- [X] T065 [P] [US3] Implement custom GHC path validation and verification in src/HsJupyter/CLI/Configuration.hs
- [X] T066 [US3] Implement KernelConfig parsing from user input in src/HsJupyter/CLI/Configuration.hs
- [X] T067 [US3] Implement ResourceLimits validation (memory, timeout, output size) in src/HsJupyter/CLI/Configuration.hs
- [X] T068 [US3] Implement custom display name handling in src/HsJupyter/CLI/Configuration.hs
- [X] T069 [US3] Implement configuration file loading (JSON format) in src/HsJupyter/CLI/Configuration.hs
- [X] T070 [US3] Implement configuration merging (CLI flags override file config) in src/HsJupyter/CLI/Configuration.hs
- [X] T071 [US3] Extend install command parser with custom configuration options in src/HsJupyter/CLI/Commands.hs
- [X] T072 [US3] Integrate custom configuration into installation workflow in src/HsJupyter/CLI/Install.hs
- [X] T073 [US3] Add validation level support (none, basic, full) in src/HsJupyter/CLI/Install.hs
- [X] T074 [US3] Implement full validation mode with kernel execution test in src/HsJupyter/CLI/Install.hs

**Checkpoint**: All core user stories should now be independently functional - basic to advanced installation scenarios work

---

## Phase 6: User Story 4 - System Integration and Verification (Priority: P3)

**Goal**: Enable programmatic installation verification and integration with automated environments through structured output and scripting support

**Independent Test**: Run installation commands in automated environment with --json flag and verify structured output enables programmatic verification and CI/CD integration

### Integration Tests for User Story 4 ⚠️

- [X] T075 [P] [US4] Integration test for JSON output validation across all commands in test/integration/CLIIntegrationSpec.hs
- [X] T076 [P] [US4] Integration test for quiet mode installation in test/integration/CLIIntegrationSpec.hs
- [X] T077 [P] [US4] Integration test for list command with multiple installations in test/integration/CLIIntegrationSpec.hs

### Unit Tests for User Story 4 ⚠️

- [X] T078 [P] [US4] Unit tests for JSON schema compliance in test/unit/CLIOutputSpec.hs
- [X] T079 [P] [US4] Unit tests for exit code correctness in test/unit/CLICommandsSpec.hs
- [X] T080 [P] [US4] Unit tests for quiet mode behavior in test/unit/CLICommandsSpec.hs

### Implementation for User Story 4

- [X] T081 [P] [US4] Implement list command to enumerate all kernel installations in src/HsJupyter/CLI/Commands.hs
- [X] T082 [P] [US4] Implement uninstall command with confirmation prompts in src/HsJupyter/CLI/Commands.hs
- [X] T083 [P] [US4] Implement version command with compatibility checking in src/HsJupyter/CLI/Commands.hs
- [X] T084 [US4] Implement quiet mode suppression of interactive prompts in src/HsJupyter/CLI/Output.hs
- [X] T085 [US4] Implement verbose mode with detailed logging output in src/HsJupyter/CLI/Output.hs
- [X] T086 [US4] Add JSON output support for install command per contracts/json-schema.md in src/HsJupyter/CLI/Output.hs
- [X] T087 [US4] Add JSON output support for doctor command per contracts/json-schema.md in src/HsJupyter/CLI/Output.hs
- [X] T088 [US4] Add JSON output support for list command per contracts/json-schema.md in src/HsJupyter/CLI/Output.hs
- [X] T089 [US4] Add JSON output support for uninstall command per contracts/json-schema.md in src/HsJupyter/CLI/Output.hs
- [X] T090 [US4] Add JSON output support for version command per contracts/json-schema.md in src/HsJupyter/CLI/Output.hs
- [X] T091 [US4] Implement proper exit code handling for all commands per contracts/cli-api.md in src/HsJupyter/CLI/Commands.hs
- [X] T092 [US4] Add global options parser (--json, --quiet, --verbose, --help) in src/HsJupyter/CLI/Commands.hs

**Checkpoint**: Complete feature implementation - all user stories functional with full automation support

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and final quality checks

- [X] T093 [P] Add comprehensive error handling examples to docs/cli-usage.md
- [X] T094 [P] Document all CLI commands and options in docs/cli-reference.md
- [X] T095 [P] Update main README.md with installation instructions using new CLI
- [X] T096 [P] Add cross-platform path handling tests in test/unit/CrossPlatformSpec.hs
- [X] T097 Run constitutional audit on all CLI modules for SOLID compliance
- [X] T098 Verify all Success Criteria from spec.md are met
- [X] T099 Run quickstart.md validation scenarios
- [X] T100 Performance profiling for installation (<30s) and diagnostics (<5s)
- [X] T101 Memory usage validation (<100MB during operations)
- [X] T102 Execute complete end-to-end workflow per quickstart.md
- [X] T103 Update CHANGELOG.md with feature description and usage examples
- [X] T104 Create migration guide for users of legacy scripts/install-kernelspec.sh

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational phase completion
- **User Story 2 (Phase 4)**: Depends on Foundational phase completion - Independent of US1
- **User Story 3 (Phase 5)**: Depends on Foundational phase completion - Extends US1
- **User Story 4 (Phase 6)**: Depends on Foundational phase completion - Integrates with all prior stories
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories ✅ MVP
- **User Story 2 (P2)**: Can start after Foundational - Independent (doctor works without install)
- **User Story 3 (P3)**: Can start after Foundational - Extends US1 (adds custom config to install)
- **User Story 4 (P4)**: Can start after Foundational - Adds integration layer across all commands

### Within Each User Story

1. **Integration tests FIRST** (write and verify they fail)
2. **Unit tests SECOND** (write and verify they fail)
3. **Implementation THIRD** (make tests pass)
4. Models/types before logic
5. Core implementation before integration
6. Story complete before moving to next priority

### Parallel Opportunities

**Within Setup (Phase 1)**:
- T003, T004, T005 can run in parallel (different modules)

**Within Foundational (Phase 2)**:
- T009, T010, T011 can run in parallel (different concerns)

**Across User Stories** (after Foundational complete):
- US1 (T016-T034) and US2 (T035-T055) can be developed in parallel by different developers
- US3 (T056-T074) extends US1, so should wait for T022-T034 core installation logic
- US4 (T075-T092) adds integration features, can partially overlap with other stories

**Within User Story 1**:
- T016, T017, T018 (integration tests) can run in parallel
- T019, T020, T021 (unit tests) can run in parallel

**Within User Story 2**:
- T035, T036, T037 (integration tests) can run in parallel
- T038, T039, T040 (unit tests) can run in parallel
- T042, T043, T044, T045, T046, T047, T048 (diagnostic checks) can run in parallel

**Within User Story 3**:
- T056, T057, T058 (integration tests) can run in parallel
- T059, T060, T061 (unit tests) can run in parallel
- T063, T064, T065 (path resolution) can run in parallel

**Within User Story 4**:
- T075, T076, T077 (integration tests) can run in parallel
- T078, T079, T080 (unit tests) can run in parallel
- T081, T082, T083 (commands) can run in parallel
- T086, T087, T088, T089, T090 (JSON formatters) can run in parallel

**Within Polish (Phase 7)**:
- T093, T094, T095, T096 (documentation and tests) can run in parallel

---

## Parallel Example: User Story 1

```bash
# Step 1: Launch all integration tests together
Task: "Integration test for default installation workflow in test/integration/CLIIntegrationSpec.hs"
Task: "Integration test for force reinstallation workflow in test/integration/CLIIntegrationSpec.hs"
Task: "Integration test for user-scoped installation workflow in test/integration/CLIIntegrationSpec.hs"

# Step 2: Launch all unit tests together
Task: "Unit tests for install command parsing in test/unit/CLICommandsSpec.hs"
Task: "Unit tests for installation scope selection in test/unit/CLIInstallSpec.hs"
Task: "Unit tests for kernelspec generation in test/unit/CLIInstallSpec.hs"

# Step 3: Implement core installation logic sequentially (has dependencies)
# Step 4: Add logging, validation, output formatting in sequence
```

---

## Implementation Strategy

### MVP First (User Story 1 Only - Fastest Path to Value)

1. Complete Phase 1: Setup (T001-T005) - ~2 hours
2. Complete Phase 2: Foundational (T006-T015) - ~4 hours
3. Complete Phase 3: User Story 1 (T016-T034) - ~8 hours
4. **STOP and VALIDATE**: Test installation on clean system
5. **Deploy/demo** if ready (basic install command working)

**Total MVP time estimate**: ~14 hours of focused development

**MVP delivers**: Users can run `hs-jupyter-kernel install` to set up kernel

### Incremental Delivery (Add Stories Progressively)

1. Complete Setup + Foundational → Foundation ready (~6 hours)
2. Add User Story 1 → Test independently → **Deploy/Demo** (MVP!) (~8 hours)
3. Add User Story 2 → Test independently → **Deploy/Demo** (diagnostics) (~6 hours)
4. Add User Story 3 → Test independently → **Deploy/Demo** (custom config) (~5 hours)
5. Add User Story 4 → Test independently → **Deploy/Demo** (automation) (~4 hours)
6. Polish phase → Final release (~3 hours)

**Total feature time estimate**: ~32 hours

Each increment adds value without breaking previous functionality.

### Parallel Team Strategy

With 3 developers after Foundational phase complete:

1. **Team completes Setup + Foundational together** (Day 1 morning)
2. **Parallel development** (Day 1 afternoon - Day 2):
   - Developer A: User Story 1 (install command)
   - Developer B: User Story 2 (doctor command)
   - Developer C: User Story 4 (JSON output, list/uninstall/version commands)
3. **Sequential completion** (Day 3):
   - Developer A: User Story 3 (custom configuration - extends install)
4. **Integration and Polish** (Day 3 afternoon):
   - All developers: Cross-cutting concerns, testing, documentation

**Total elapsed time with 3 developers**: ~2.5 days

---

## Task Estimation Summary

- **Phase 1 (Setup)**: 5 tasks, ~2 hours
- **Phase 2 (Foundational)**: 10 tasks, ~4 hours
- **Phase 3 (US1 - Installation)**: 19 tasks, ~8 hours
- **Phase 4 (US2 - Diagnostics)**: 21 tasks, ~6 hours
- **Phase 5 (US3 - Custom Config)**: 19 tasks, ~5 hours
- **Phase 6 (US4 - Integration)**: 18 tasks, ~4 hours
- **Phase 7 (Polish)**: 12 tasks, ~3 hours

**Total**: 104 tasks, ~32 hours estimated

---

## Notes

- [P] tasks = different files, no dependencies - can run in parallel
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- Tests written FIRST and must FAIL before implementation begins
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Constitutional compliance required: ResourceGuard, katip logging, structured errors

## Constitution Compliance Checklist

Every implementation task must satisfy:

### Core Principles (from HsJupyter Constitution v1.2.0)

- **Documentation-First (I)**: All user stories have acceptance tests defined
- **Test-First (II)**: Integration and unit tests precede implementation for each story
- **Specification-Driven (III)**: Following speckit.tasks workflow from validated spec
- **Observability (IV)**: Structured logging via katip for all operations, diagnostic data in doctor command
- **Modular Architecture (V)**: Separate modules for Commands, Install, Doctor, Output, Configuration, Utilities (Single Responsibility), CLI abstraction enables extension (Open/Closed), Option types separate concerns (Interface Segregation), Pure functions with dependency injection (Dependency Inversion)
- **Simplicity (VI)**: DRY shared utilities, KISS minimal CLI surface, YAGNI only spec requirements
- **Resilience (VII)**: Path validation, graceful Jupyter missing handling, Either/Maybe for fallible operations, CLIDiagnostic structured errors, minimal coupling through option records
- **Pragmatic Balance (VIII)**: Rule of Three - refactor on repetition, high cohesion within CLI modules, loose coupling to kernel

### Implementation Requirements

- **Error Handling**: Extend RuntimeDiagnostic, use CLIDiagnostic types
- **Logging**: katip structured logging with correlation IDs for all operations
- **Resource Management**: ResourceGuard for installation timeout and disk space limits
- **Cancellation**: TMVar-based cancellation for long-running operations
- **Cross-Platform**: Use System.FilePath, handle Windows/Unix differences
- **Performance**: Installation <30s with dependencies, diagnostics <5s, memory <100MB

### Testing Requirements

- Integration tests verify end-to-end workflows
- Unit tests verify individual functions and modules
- Cross-platform compatibility tests (Linux, macOS, Windows)
- JSON output schema validation
- Constitutional compliance validation (resource limits, logging, errors)
