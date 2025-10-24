# Implementation Plan: GHC Evaluation

**Branch**: `003-ghc-evaluation` | **Date**: 2024-10-25 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/003-ghc-evaluation/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Transform HsJupyter from an advanced echo server into a functional Haskell REPL by integrating real GHC evaluation capabilities using the `hint` library. This phase replaces echo-based evaluation with actual Haskell compilation and execution, enabling persistent state across cells, module imports, and comprehensive error handling while maintaining all existing Phase 2 infrastructure (STM concurrency, resource limits, cancellation).

## Technical Context

<!--
  ACTION REQUIRED: Replace the content in this section with the technical details
  for the project. The structure here is presented in advisory capacity to guide
  the iteration process.
-->

**Language/Version**: Haskell with GHC 9.12.2 via ghcup  
**Primary Dependencies**: hint >= 0.9.0 (GHC API), zeromq4-haskell, aeson, katip, stm  
**Storage**: In-memory interpreter state (hint InterpreterT monad)  
**Testing**: hspec test suite (unit + integration), existing 78 tests must continue passing  
**Target Platform**: Linux server (development), cross-platform Haskell runtime
**Project Type**: Single project (library integration into existing kernel)  
**Performance Goals**: Simple expressions <200ms (1s timeout), imports <2s (5s timeout), complex computations (10s timeout)  
**Constraints**: <100MB memory baseline, maintain STM concurrency, TMVar cancellation, ResourceGuard limits  
**Scale/Scope**: Single-user REPL sessions, standard library imports, persistent state across notebook cells

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**POST-DESIGN VALIDATION**: ✅ All gates remain satisfied after Phase 1 design completion

| Gate | Status | Notes |
|------|--------|-------|
| Documentation-first: spec and plan must precede implementation | ✅ | Complete spec.md with clarifications, plan.md in progress |
| Test-first mindset: define acceptance & soak tests before runtime work | ✅ | 5 user stories with acceptance scenarios, performance targets defined |
| Specification-driven development: follow speckit workflow | ✅ | Following speckit.specify → speckit.plan → speckit.tasks workflow |
| Observability foundation: structured logging and diagnostics | ✅ | Leverage existing katip logging, RuntimeDiagnostic system, telemetry |
| Modular architecture: maintain HsJupyter.* namespace | ✅ | New GHCRuntime under HsJupyter.Runtime.*, preserve STM patterns |
| Simplicity & maintainability: apply DRY, KISS, YAGNI principles | ✅ | Replace echo with hint (simplest GHC integration), no speculative features |

## Project Structure

### Documentation (this feature)

```text
specs/003-ghc-evaluation/
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
src/HsJupyter/
├── Runtime/
│   ├── GHCRuntime.hs         # NEW: hint-based evaluation engine
│   ├── GHCSession.hs         # NEW: persistent interpreter state
│   ├── GHCDiagnostics.hs     # NEW: GHC error mapping
│   ├── EchoRuntime.hs        # EXISTING: to be replaced/deprecated
│   ├── Manager.hs            # EXISTING: integrate GHCRuntime
│   └── SessionState.hs       # EXISTING: extend for GHC state
├── Kernel/
│   └── Types.hs              # EXISTING: may need GHC-specific types
└── Bridge/                   # EXISTING: no changes required
    └── ...

test/
├── unit/
│   ├── GHCRuntimeSpec.hs     # NEW: GHC evaluation unit tests
│   ├── GHCSessionSpec.hs     # NEW: state persistence tests
│   └── GHCDiagnosticsSpec.hs # NEW: error handling tests
└── integration/
    └── GHCNotebookSpec.hs    # NEW: end-to-end GHC workflow
```

**Structure Decision**: Single project structure extending existing HsJupyter.Runtime.* namespace. New GHC components integrate with existing STM-based architecture while preserving all Phase 2 infrastructure (cancellation, resource limits, diagnostics).

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No constitution violations identified. All complexity is justified by requirements:

- hint library integration: Simplest path to GHC evaluation vs raw GHC API
- New modules: Required for GHC functionality, following existing namespace patterns
- STM integration: Preserves existing concurrency model, maintains consistency

---

## Plan Status

**Phase 0**: ✅ Complete - Research resolved all technical unknowns  
**Phase 1**: ✅ Complete - Design artifacts generated (data-model.md, contracts/, quickstart.md)  
**Phase 2**: ⏳ Ready for `/speckit.tasks` to generate implementation tasks  

**Generated Artifacts**:

- `research.md` - hint integration patterns, resource management, state persistence
- `data-model.md` - Complete entity model with validation rules and integration points  
- `contracts/api-specification.md` - Haskell module interfaces and backwards compatibility
- `quickstart.md` - Step-by-step implementation guide with testing strategy

**Next Command**: `/speckit.tasks` to break down implementation into concrete tasks
