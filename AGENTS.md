# Repository Guidelines

## Project Structure & Module Organization

HsJupyter is documentation-first. Use `docs/architecture.md`, `docs/roadmap.md`, and `docs/developer/` to capture design decisions and contributor process updates. Stage implementation sketches on `NNN-slug` branches mirroring components (`KernelProcess`, `JupyterBridge`, `RuntimeManager`) to keep the eventual `src/` tree predictable. Place production Haskell modules under `src/HsJupyter/`, mirror tests in `test/`, and keep prototype scripts in `.specify/scripts/` with executable bits set. Specification artefacts for each phase live in `specs/`, where folders like `001-protocol-bridge` and `002-runtime-core` hold spec, plan, and task files that stay aligned with implementation.

## Build, Test, and Development Commands

Install GHC 9.6.4 and Cabal via `ghcup` (e.g. `ghcup install ghc 9.6.4 cabal`). Use `cabal v2-build` for compilation, `cabal v2-repl` for interactive sketches, and run `cabal v2-test` before sending changes. Lint docs with `markdownlint docs/**/*.md README.md`. Prototype runners live under `.specify/scripts/` and should be invoked from repo root (e.g. `.specify/scripts/dev-kernel.sh --json`).

## Coding Style & Naming Conventions

Indent Haskell with four spaces and keep every module under the `HsJupyter.*` namespace. Prefer descriptive singular record types (`KernelProcessConfig`) and camel-cased constructors. Lean on pure, total functions; add brief Haddock comments for non-obvious control flow or bridging logic. If you introduce formatting tooling, document the choice in the same change set.

## Testing Guidelines

Adopt `hspec` suites that mirror the module tree (`test/RequestRouterSpec.hs`, etc.). Provide deterministic repro steps for interim prototypes and add expected output snippets. When runtime behaviour evolves, extend tests first, then wire the matching implementation. Capture coverage notes or log excerpts when CI is unavailable.

## Commit & Pull Request Guidelines

Write imperative commit subjects (`Add runtime manager sketch`) with bodies focused on rationale, follow-ups, or doc impacts. Reference roadmap checklist entries in `docs/roadmap.md` and link related issues. PRs should outline behavioural changes, enumerate documentation updates, and attach screenshots, logs, or command transcripts for user-visible work. Track open questions as Markdown checklists so reviewers can respond explicitly.

## Agent Workflow & Prompt Usage

Codex agents must review the Specify prompts in `.codex/prompts/` before invoking `/speckit`. Execute helper scripts from the repo root, keep generated checklists current, and sync results with the relevant `specs/` tasks before moving phases.

## Specification Assets

Treat `.specify/` as the automation workspace: `templates/` holds authoring scaffolds, `scripts/` houses helper binaries, and `memory/` stores constitution notes. When advancing roadmap work, update the matching `specs/<phase>/tasks.md` entries alongside code changes and regenerate any `.specify` checklists or plans that track their completion.
