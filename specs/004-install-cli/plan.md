# Implementation Plan: Installation & CLI Infrastructure

**Branch**: `004-install-cli` | **Date**: 2025-01-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/004-install-cli/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Create command-line installation and management tools for HsJupyter kernel integration with Jupyter environments. Primary requirement: enable users to install, configure, and diagnose HsJupyter kernel installations through simple CLI commands (`install`, `doctor`, `uninstall`). Technical approach: extend existing kernel executable with CLI subcommands that integrate with Jupyter's kernelspec system, leveraging constitutional error handling and observability patterns.

## Technical Context

<!--
  ACTION REQUIRED: Replace the content in this section with the technical details
  for the project. The structure here is presented in advisory capacity to guide
  the iteration process.
-->

**Language/Version**: Haskell with GHC 9.12.2+ via ghcup  
**Primary Dependencies**: existing HsJupyter kernel, process, filepath, directory, unix (for system integration), optparse-applicative (CLI parsing)  
**Storage**: filesystem-based (Jupyter kernelspec directories, kernel.json files)  
**Testing**: hspec for unit and integration tests, following existing constitutional patterns  
**Target Platform**: Linux, macOS, Windows (cross-platform Jupyter integration)
**Project Type**: single project (CLI extension to existing kernel)  
**Performance Goals**: <2 minutes for installation, <5 seconds for diagnostics, <30 seconds for operations with dependencies available  
**Constraints**: <100MB temporary disk usage during installation, must integrate with existing constitutional error handling and observability  
**Scale/Scope**: single-user installations, integration with standard Jupyter environments (Lab, Notebook, kernelspec system)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Post-Phase 1 Re-evaluation**: All gates continue to pass with completed design artifacts.

| Gate | Status | Notes |
|------|--------|-------|
| Documentation-first: spec and plan must precede implementation | ✅ | Comprehensive spec with 4 prioritized user stories and 15 functional requirements complete |
| Test-first mindset: define acceptance & soak tests before runtime work | ✅ | Acceptance scenarios defined for each user story, performance targets specified (<2min install, <5s diagnostics) |
| Specification-driven development: follow speckit workflow | ✅ | Currently in /speckit.plan phase following proper workflow |
| Observability foundation: structured logging and diagnostics | ✅ | Will integrate with existing katip structured logging, leverage Runtime/Telemetry module |
| Modular architecture & strong design: apply SOLID principles, composition over inheritance | ✅ | CLI commands as composable functions, separate concerns for installation/diagnostics/configuration |
| Simplicity & maintainability: apply DRY, KISS, YAGNI principles | ✅ | Minimal CLI extension to existing kernel, only implement explicitly required user stories |
| Resilience & defensive programming: error handling, Law of Demeter | ✅ | Leverage existing RuntimeDiagnostic system, validate inputs, handle Jupyter environment edge cases |
| Pragmatic balance: Rule of Three, cohesion/coupling balance | ✅ | Single CLI extension with focused responsibilities, minimal coupling to Jupyter internals |

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)
<!--
  ACTION REQUIRED: Replace the placeholder tree below with the concrete layout
  for this feature. Delete unused options and expand the chosen structure with
  real paths (e.g., apps/admin, packages/something). The delivered plan must
  not include Option labels.
-->

```text
# HsJupyter CLI Extension (Single project extension)
src/HsJupyter/
├── CLI/                    # New CLI module tree
│   ├── Commands.hs         # CLI command definitions and parsing  
│   ├── Install.hs          # Installation logic and kernelspec integration
│   ├── Doctor.hs           # Diagnostic and troubleshooting commands
│   ├── Config.hs           # Configuration management and validation
│   └── System.hs           # System integration utilities (Jupyter detection, paths)
├── Kernel/                 # Existing kernel infrastructure
└── Runtime/                # Existing runtime with ErrorHandling.hs integration

app/
└── KernelMain.hs           # Extended to support CLI subcommands

test/
├── integration/
│   └── CLIIntegrationSpec.hs    # End-to-end CLI workflow tests
└── unit/
    ├── CLICommandsSpec.hs       # Command parsing and validation
    ├── InstallSpec.hs           # Installation logic unit tests  
    ├── DoctorSpec.hs            # Diagnostic functionality tests
    └── SystemIntegrationSpec.hs # Jupyter environment detection tests
```

**Structure Decision**: Single project extension leveraging existing HsJupyter architecture. CLI functionality added as new module tree under `src/HsJupyter/CLI/` with integration tests following established patterns. Main executable extended to support subcommands while preserving existing kernel functionality.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

*No constitutional violations detected. All gates pass with current approach.*
