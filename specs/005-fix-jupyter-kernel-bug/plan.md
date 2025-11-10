# Implementation Plan: Jupyter Kernel Integration Bug Fix

**Branch**: `005-fix-jupyter-kernel-bug` | **Date**: 2025-11-04 | **Spec**: [link](./spec.md)
**Input**: Feature specification from `/specs/005-fix-jupyter-kernel-bug/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

This plan outlines the technical approach to fix a critical bug preventing the HsJupyter kernel from integrating with Jupyter environments. The primary goal is to ensure the kernel can be discovered, started, and used for code execution within Jupyter Notebook and JupyterLab. The fix will focus on aligning the kernel's communication with the Jupyter messaging protocol (ZMQ) and ensuring proper lifecycle management (startup, shutdown, interrupts).

## Technical Context

**Language/Version**: Haskell (GHC 9.12.2+)
**Primary Dependencies**: `zeromq4-haskell`, `aeson`, `katip`, `stm`
**Storage**: N/A (in-memory kernel state)
**Testing**: `hspec` for unit and integration tests
**Target Platform**: Linux, macOS, Windows (wherever Jupyter is supported)
**Project Type**: Single project (kernel library and executable)
**Performance Goals**: Kernel startup <5 seconds, simple expression execution <500ms (aligned with research.md and competitive with other Jupyter kernels like IJulia, IRKernel)
**Constraints**: Must be compatible with Jupyter Notebook v6.x and JupyterLab v3.x.
**Scale/Scope**: Single-user kernel, not designed for concurrent remote access.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Gate | Status | Notes |
|------|--------|-------|
| Documentation-first: spec and plan must precede implementation | ✅ | Specification and this plan are in place. |
| Test-first mindset: define acceptance & soak tests before runtime work | ✅ | Acceptance scenarios from the spec will be converted to `hspec` tests. |
| Specification-driven development: follow speckit workflow | ✅ | Following the `/speckit.specify` -> `/speckit.plan` workflow. |
| Observability foundation: structured logging and diagnostics | ✅ | `katip` will be used for structured logging to diagnose integration issues. |
| Modular architecture & strong design: apply SOLID principles, composition over inheritance | ✅ | The existing modular design (`Bridge`, `Runtime`, `Router`) will be leveraged. |
| Simplicity & maintainability: apply DRY, KISS, YAGNI principles | ✅ | The fix will be focused and minimal, avoiding unnecessary complexity. |
| Resilience & defensive programming: error handling, Law of Demeter | ✅ | Robust error handling for protocol messages and kernel lifecycle events is a primary goal. |
| Pragmatic balance: Rule of Three, cohesion/coupling balance | ✅ | The solution will be pragmatic, focusing on fixing the immediate issue without major refactoring. |

## Project Structure

### Documentation (this feature)

```text
specs/005-fix-jupyter-kernel-bug/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)
```text
# Single project (DEFAULT)
src/
├── HsJupyter/
    ├── Bridge/
    ├── CLI/
    ├── Kernel/
    ├── Router/
    └── Runtime/

test/
├── integration/
└── unit/
```

**Structure Decision**: The existing single project structure is appropriate and will be maintained. The fix will likely involve changes within the `src/HsJupyter/Bridge` and `src/HsJupyter/Kernel` modules.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A       | N/A        | N/A                                 |
