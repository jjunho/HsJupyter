# Implementation Plan: Installation & CLI Infrastructure

**Branch**: `004-install-cli` | **Date**: 2025-11-14 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/004-install-cli/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Provide command-line installation and management tools for the HsJupyter kernel, enabling users to install, diagnose, and configure the kernel with Jupyter environments. The feature includes `install`, `doctor`, `uninstall`, and `list` commands with support for custom configurations, multiple installation scopes (user/system), and comprehensive diagnostics. Implementation leverages existing constitutional patterns (katip logging, resource guards, structured error handling) while adding filesystem integration for Jupyter kernelspec directories and cross-platform path management.

## Technical Context

**Language/Version**: Haskell with GHC 9.6.7+ (9.12.2+ recommended) via ghcup  
**Primary Dependencies**: `optparse-applicative` (CLI parsing), `filepath`, `directory`, `unix` (for system integration), `process` (subprocess execution), `aeson` (JSON output), existing HsJupyter kernel modules  
**Storage**: Filesystem-based (Jupyter kernelspec directories, kernel.json files)  
**Testing**: `hspec` for unit and integration tests, mirroring module tree under `test/unit/` and `test/integration/`  
**Target Platform**: Linux (primary), macOS (secondary), Windows (tertiary) - cross-platform paths via `filepath`, platform-specific logic via `unix` package conditionals  
**Project Type**: Single project (Haskell library + executable) extending existing kernel codebase  
**Performance Goals**: Installation <30s with dependencies available, diagnostics <5s, all CLI operations complete within constitutional resource limits  
**Constraints**: <100MB disk space during installation, graceful degradation when Jupyter not found, no network dependencies (local-only operations)  
**Scale/Scope**: Single-user CLI tool, ~2000 LOC across 7 CLI modules, 15 functional requirements with corresponding tests

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Gate | Status | Notes |
|------|--------|-------|
| Documentation-first: spec and plan must precede implementation | ✅ | Spec complete and validated (specs/004-install-cli/spec.md), plan in progress, all documentation precedes implementation |
| Test-first mindset: define acceptance & soak tests before runtime work | ✅ | All user stories have acceptance scenarios, test structure mirrors spec requirements, integration tests planned for each command |
| Specification-driven development: follow speckit workflow | ✅ | Following speckit.plan workflow, on branch 004-install-cli, spec validated, proceeding to plan → tasks → implement |
| Observability foundation: structured logging and diagnostics | ✅ | Reuse existing katip logging infrastructure, structured JSON output via --json flag, diagnostics via doctor command with RuntimeDiagnostic patterns |
| Modular architecture & strong design: apply SOLID principles, composition over inheritance | ✅ | Separate modules for Commands, Install, Doctor, Output, Types, Configuration. Single Responsibility per module, Open/Closed via command abstraction, Interface Segregation via Options types, Dependency Inversion via pure functions |
| Simplicity & maintainability: apply DRY, KISS, YAGNI principles | ✅ | Minimal CLI surface (4 commands), shared utilities in CLI.Utilities, no speculative features, leverage existing kernel infrastructure |
| Resilience & defensive programming: error handling, Law of Demeter | ✅ | Validate all file paths, handle missing Jupyter gracefully, use Either/Maybe for fallible operations, structured errors via CLI.Types, minimal coupling through option records |
| Pragmatic balance: Rule of Three, cohesion/coupling balance | ✅ | Refactoring deferred until repetition emerges, high cohesion within CLI modules, loose coupling to kernel via existing public APIs |

## Project Structure

### Documentation (this feature)

```text
specs/004-install-cli/
├── spec.md              # ✅ Feature specification (complete)
├── plan.md              # ✅ This file (/speckit.plan command output - complete)
├── research.md          # ✅ Phase 0 output (complete)
├── data-model.md        # ✅ Phase 1 output (complete)
├── quickstart.md        # ✅ Phase 1 output (complete)
├── contracts/           # ✅ Phase 1 output (complete)
│   ├── cli-api.md       # ✅ CLI command contracts
│   └── json-schema.md   # ✅ JSON output schemas
├── checklists/
│   └── requirements.md  # ✅ Spec validation checklist (complete)
└── tasks.md             # ⏳ Phase 2 output (/speckit.tasks command - next step)
```

### Source Code (repository root)

```text
src/HsJupyter/CLI/
├── Commands.hs          # ✅ Command parsing and routing (partial implementation exists)
├── Types.hs             # ✅ CLI data types (InstallScope, ValidationLevel, ResourceLimits)
├── Install.hs           # ✅ Installation logic (partial implementation exists)
├── Doctor.hs            # ✅ Diagnostic commands (partial implementation exists)
├── Output.hs            # ✅ Structured output formatting (JSON/human-readable)
├── Utilities.hs         # ✅ Shared utilities (path resolution, platform detection)
└── Configuration.hs     # ✅ Configuration file management (partial implementation exists)

app/
└── KernelMain.hs        # ✅ Main entry point (needs CLI integration)

test/unit/
├── CLICommandsSpec.hs   # ✅ Command parsing tests (exists)
├── CLIInstallSpec.hs    # ✅ Installation logic tests (exists)
├── CLITypesSpec.hs      # ✅ Type validation tests (exists)
├── DoctorSpec.hs        # ✅ Doctor command tests (exists)
└── CrossPlatformSpec.hs # ✅ Platform-specific path tests (exists)

test/integration/
└── CLIIntegrationSpec.hs # ✅ End-to-end CLI tests (exists)

scripts/
└── install-kernelspec.sh # ✅ Legacy installation script (reference for behavior)

kernelspec/hsjupyter/
└── kernel.json.template  # ✅ Jupyter kernelspec template (exists)
```

**Structure Decision**: Single project structure (Option 1 from template). HsJupyter is a unified Haskell library + executable, not a web/mobile application. CLI modules live under `src/HsJupyter/CLI/` following the established namespace pattern. Tests mirror source structure under `test/unit/` and `test/integration/`. Existing partial implementations in CLI modules indicate work-in-progress status - this plan consolidates and completes the feature.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., 4th project] | [current need] | [why 3 projects insufficient] |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient] |

No constitutional violations. All gates passed.

## Phase Completion Status

### Phase 0: Research ✅ COMPLETE

All technical unknowns resolved and documented in [research.md](research.md):
- Jupyter kernelspec integration strategy
- Cross-platform path handling approach
- CLI framework selection (optparse-applicative)
- Diagnostic check implementations
- Configuration management design
- Resource limit enforcement patterns
- Error handling strategy
- Testing approach

### Phase 1: Design & Contracts ✅ COMPLETE

All design artifacts generated:
- ✅ [data-model.md](data-model.md) - Entity definitions and relationships
- ✅ [contracts/cli-api.md](contracts/cli-api.md) - CLI command contracts
- ✅ [contracts/json-schema.md](contracts/json-schema.md) - JSON output schemas
- ✅ [quickstart.md](quickstart.md) - Developer usage guide
- ✅ AGENTS.md updated with new technologies

### Next Phase: Tasks ⏳ PENDING

Ready to proceed with `/speckit.tasks` command to generate:
- Implementation tasks breakdown
- Work estimation
- Dependency mapping
- Test coverage requirements

## Post-Phase 1 Constitution Re-check

*Re-evaluation after design completion:*

| Gate | Status | Updated Notes |
|------|--------|---------------|
| Documentation-first | ✅ | All planning artifacts complete (spec, plan, research, data-model, contracts, quickstart) |
| Test-first mindset | ✅ | Test structure defined in data-model, acceptance scenarios mapped to test cases |
| Specification-driven | ✅ | Completed /speckit.specify → /speckit.plan, ready for /speckit.tasks |
| Observability | ✅ | Diagnostic entity defined with CheckStatus levels, JSON output schema complete |
| Modular architecture | ✅ | Entity relationships minimize coupling, clear separation of concerns in data model |
| Simplicity (DRY/KISS/YAGNI) | ✅ | Entities reuse existing patterns, no over-engineering, focused on spec requirements |
| Resilience | ✅ | CLIError taxonomy complete with actionable messages, validation strategies defined |
| Pragmatic balance | ✅ | Abstraction levels appropriate for current scope, refactoring opportunities noted |

**Verdict**: All constitutional gates remain PASSED after Phase 1 design. No violations introduced.

## Summary

Implementation plan for Phase 4: Installation CLI is complete through Phase 1 design. The feature extends the existing HsJupyter kernel with comprehensive CLI tools for installation, diagnostics, and configuration management.

**Key Decisions**:
- Leverage existing dependencies (no new additions required)
- Reuse constitutional patterns (ResourceGuard, katip logging, structured errors)
- Cross-platform support via standard libraries (filepath, directory)
- Filesystem-based integration with Jupyter kernelspec system

**Ready for**: `/speckit.tasks` command to generate implementation task breakdown
