<!--
Sync Impact Report:
- Version change: 1.1.0 → 1.2.0 (enhanced principle definitions with resilience and pragmatic balance)
- Added principles: VII. Resilience & Defensive Programming (defensive programming, Law of Demeter), VIII. Pragmatic Balance (Rule of Three, cohesion/coupling)
- Modified principles: V. Modular Architecture (enhanced with SOLID principles, composition over inheritance), VI. Simplicity & Maintainability (expanded with separation of concerns)
- Added sections: Enhanced design foundation guidance, implementation details hiding
- Removed sections: None
- Templates requiring updates: ✅ plan-template.md (updated constitution check gates with SOLID principles, defensive programming), ✅ tasks-template.md (updated constitution guidance with all four principles)
- Follow-up TODOs: None - all principles implemented with comprehensive guidance
-->

# HsJupyter Constitution

## Core Principles

### I. Documentation-First Development

Every feature begins with comprehensive documentation in `specs/` before any implementation work. Architecture decisions live in `docs/architecture.md`, roadmap updates in `docs/roadmap.md`, and contributor processes in `docs/developer/`. Specification artifacts (spec.md, plan.md, research.md, data-model.md, contracts/, quickstart.md) MUST be complete and validated before proceeding to task generation. Code reviews reject work that lacks matching documentation or leaves design decisions undocumented.

### II. Test-First Implementation  

Tests MUST be written before implementation and MUST fail before code is written to make them pass. Mirror the module tree under `test/` with files following the pattern `ModuleNameSpec.hs`. Run `cabal v2-test` before opening any PR. Unit tests for all new modules, integration tests for user-facing features, and golden tests for protocol compatibility are mandatory. Test scenarios from user story acceptance criteria become automated tests.

### III. Specification-Driven Development

Follow the speckit workflow rigidly: `/speckit.specify` → `/speckit.plan` → `/speckit.tasks` → `/speckit.implement`. Feature branches follow `NNN-feature-name` naming. Each phase (spec, plan, tasks) MUST be complete before proceeding to the next. User stories MUST be prioritized (P1, P2, P3) and independently testable. All acceptance scenarios become test cases.

### IV. Observability Foundation

Structured logging, metrics collection, and diagnostic reporting are mandatory from the earliest phases. Use `katip` for structured JSON logs with correlation IDs. Expose telemetry through the `Runtime/Telemetry.hs` module. Every runtime operation MUST support cancellation via TMVar tokens and resource monitoring via `ResourceGuard`. Error handling MUST use the structured `RuntimeDiagnostic` system with severity classification.

### V. Modular Architecture & Strong Design Foundation

Maintain the `HsJupyter.*` namespace with clear module separation: `Bridge/` for protocol integration, `Runtime/` for execution core, `Router/` for message dispatch, `Kernel/` for types. Apply SOLID principles to create modular, flexible, and testable systems. Each module MUST minimize dependencies and maximize clarity. Use composition over inheritance - combine small, reusable components rather than rigid hierarchies. Enforce separation of concerns to keep each module focused on a single responsibility. Use STM for thread-safe state management. Prefer total functions over partial functions. Every module MUST have comprehensive Haddock documentation with implementation details properly hidden behind clean interfaces. Follow four-space indentation and descriptive naming conventions.

### VI. Simplicity & Maintainability

Apply DRY (Don't Repeat Yourself), KISS (Keep It Simple, Stupid), and YAGNI (You Aren't Gonna Need It) principles rigorously. Eliminate code duplication through shared utilities and type-safe abstractions. Choose the simplest solution that meets requirements - complex patterns MUST be justified with concrete benefits. Implement only features explicitly required by current user stories; speculative features are forbidden. Maintain strict separation of concerns across all modules and layers. Refactor ruthlessly to maintain clarity. When complexity is unavoidable, isolate it behind clean interfaces with comprehensive documentation.

### VII. Resilience & Defensive Programming

Anticipate and handle potential failures gracefully through comprehensive error handling and resource management. Apply the Law of Demeter to reduce tight coupling between components, keeping code modular and maintainable. Every public API MUST validate inputs and handle edge cases explicitly. Use structured error types (`RuntimeDiagnostic`) rather than throwing exceptions. Implement proper resource cleanup through `ResourceGuard` and bracketing patterns. All network operations, file I/O, and external process interactions MUST include timeout handling and graceful degradation. Design systems to fail safely and provide meaningful diagnostic information.

### VIII. Pragmatic Balance & Evolution

Apply the Rule of Three - don't refactor until repetition proves a pattern is worth abstracting. Maximize cohesion within modules while minimizing coupling between them to keep components self-contained yet cleanly interacting. Balance optimization with simplicity - premature optimization is forbidden, but performance requirements MUST be specified and validated. Make architectural decisions based on concrete evidence rather than speculation. When refactoring, preserve existing interfaces unless breaking changes provide substantial benefits. Document trade-offs explicitly in design decisions.

## Development Workflow & Quality Gates

### Branching Strategy

Create numbered feature branches (`003-ghc-evaluation`) with meaningful commits in imperative mood. Each commit MUST summarize behavior changes and reference roadmap items. PRs MUST include coverage reports, demo steps, and flag open questions as checklists.

### Quality Standards  

- Haskell code MUST follow project style guidelines (four-space indentation, `HsJupyter.*` namespace)
- All public APIs MUST have Haddock comments
- Performance requirements MUST be specified and validated (e.g., <200ms evaluation, <2s startup)
- Resource constraints MUST be enforced (CPU, memory, output limits)
- Error scenarios MUST be comprehensively tested

### Implementation Phases

- **Phase 1**: Setup and infrastructure
- **Phase 2**: Foundational prerequisites (blocking - no user stories until complete)  
- **Phase 3+**: User stories in priority order (P1, P2, P3)
- **Final Phase**: Polish, documentation, and cross-cutting concerns

## Technology Standards

### Core Stack

- **Language**: Haskell with GHC 9.12.2+ via ghcup
- **Concurrency**: STM for state management, TMVar for cancellation
- **Protocol**: ZeroMQ (`zeromq4-haskell`) for Jupyter integration
- **JSON**: `aeson` for serialization
- **Testing**: `hspec` for unit and integration tests
- **Logging**: `katip` for structured logging

### Performance Targets

- Simple operations: <200ms response time
- Session initialization: <2 seconds  
- Memory baseline: <100MB for typical workflows
- Resource limits: CPU, memory, and output monitoring mandatory

### Integration Requirements

All new features MUST maintain compatibility with existing Phase 1 (Protocol Bridge) and Phase 2 (Runtime Core) infrastructure. Preserve STM-based job queues, TMVar cancellation, ResourceGuard limits, and diagnostic reporting.

## Governance

This constitution supersedes all other development practices. Amendments require documentation of rationale, approval from maintainers, and a migration plan for affected artifacts. All PRs and code reviews MUST verify compliance with these principles. Complexity that violates principles MUST be explicitly justified with simpler alternatives documented as rejected.

For runtime development guidance, reference `AGENTS.md` for agent workflow specifics and `.specify/` scripts for tooling usage.

**Version**: 1.2.0 | **Ratified**: 2025-10-25 | **Last Amended**: 2025-01-28
