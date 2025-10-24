<!--
Sync Impact Report
Version: 0.0.0 → 1.0.0
Modified Principles:
- Principle I → Documentation-First Planning
- Principle II → Specification Discipline
- Principle III → Test-First Execution
- Principle IV → Observability & Safety
- Principle V → Simplicity & Reproducibility
Added Sections:
- Additional Constraints
- Development Workflow & Quality Gates
Removed Sections:
- None
Templates:
- .specify/templates/plan-template.md ✅ updated
- .specify/templates/spec-template.md ✅ updated
- .specify/templates/tasks-template.md ✅ updated
Follow-ups:
- None
-->

# HsJupyter Constitution

## Core Principles

### I. Documentation-First Planning
Every feature must begin with documented research, specs, data models, and roadmap updates in `specs/<phase>/`. Code reviews reject work that lacks matching documentation or leaves diagrams unchecked. Docs stay authoritative—when behaviour changes, update the spec before or alongside implementation.

### II. Specification Discipline
Agents MUST execute the `.codex/prompts/speckit.*` guides before running `/speckit` commands and keep generated plans, tasks, and checklists in sync with the implementation status. Feature branches follow `NNN-slug` naming and update the corresponding `specs/<phase>/tasks.md` as validation gates move.

### III. Test-First Execution
Tests lead every change: write or extend unit, integration, and documentation tests so they fail before code passes. The full suite (`cabal v2-test`, integration notebooks, `markdownlint`) MUST run locally prior to review, with repro steps captured for any temporary gaps.

### IV. Observability & Safety
Structured logging, diagnostics, telemetry, and resource guards are non-negotiable. New runtime behaviour MUST declare metrics, log fields, and safety limits; cancellation and error paths must be exercised in tests. Instrumentation belongs in the same PR as the behaviour it observes.

### V. Simplicity & Reproducibility
Prefer minimal, composable solutions adhering to DRY/KISS/YAGNI. Use the ghcup-managed GHC toolchain and pinned Cabal configuration to keep builds deterministic. New dependencies or architectural complexity require documented justification in specs and plans.

## Additional Constraints

- Tooling: Use `ghcup install ghc 9.6.4 cabal` and document any extra flags or scripts in the PR.
- Scripts: Place prototypes under `.specify/scripts/` with executable bits and usage notes; keep automation assets in `.specify/` synced with specs.
- Branch Hygiene: Keep feature branches focused; rebase or merge only after specs/tests reflect the current state.
- Documentation QoS: Run `markdownlint docs/**/*.md README.md` and ensure diagrams or tables change with their narrative.

## Development Workflow & Quality Gates

1. Phase 0 research → summarize findings in `specs/<phase>/research.md`.
2. Phase 1 design → update `plan.md`, `data-model.md`, `quickstart.md`, and contracts.
3. Run `/speckit.tasks` once design artefacts are complete; keep task checklists in lockstep with implementation.
4. For each user story: write failing tests, implement behaviour, add telemetry, update docs.
5. Before merge: rerun full test + lint suite, update specs/tasks, note outstanding TODOs as tracked checklist items.

## Governance

- Amendments require consensus in review plus simultaneous updates to affected templates and specs.
- Versioning follows semantic rules: MAJOR for principle changes, MINOR for new principles/sections, PATCH for clarifications.
- Compliance is verified during plan (Constitution Check), code review, and release readiness; violations must be documented in the plan’s complexity log with mitigation.
- Store Sync Impact Reports at the top of this file for traceability.

**Version**: 1.0.0 | **Ratified**: 2025-10-24 | **Last Amended**: 2025-10-24
