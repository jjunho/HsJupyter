# Repository Guidelines

## Project Structure & Module Organization

HsJupyter is documentation-first while the kernel takes shape. Keep architecture decisions in `docs/architecture.md`, roadmap updates in `docs/roadmap.md`, and contributor process notes in `docs/developer/`. When adding implementation drafts, stage them under clearly named branches and mirror the planned components (`KernelProcess`, `JupyterBridge`, `RuntimeManager`) so the eventual `src/` tree remains predictable. Update diagrams or reference tables alongside the written guidance they support.

## Build, Test, and Development Commands

Use a GHC toolchain installed via `ghcup` (`ghcup install ghc 9.12.2 cabal`) before standing up prototypes.

**⚡ Performance Note**: Full builds take several minutes due to the hint library (GHC API). For faster development:

- `cabal build lib:hs-jupyter-kernel -O0` (5 seconds vs minutes)
- `cabal test unit -O0 --test-option="--match=/ModuleName/"` (targeted tests)
- Configure `jobs: 4` and `documentation: False` in cabal.project for parallel builds

Build emerging packages with `cabal v2-build` or `cabal v2-repl` and record any extra flags in your PR. Run documentation checks locally—`markdownlint docs/**/*.md README.md`—to keep the published guides consistent. Prototype scripts (e.g., ZeroMQ harnesses) should live under `.specify/scripts/` with executable bits set (`chmod +x .specify/scripts/dev-kernel.sh`) and usage documented.

## Coding Style & Naming Conventions

Follow four-space indentation for Haskell and keep modules under the `HsJupyter.*` namespace. Prefer descriptive, singular record names (`KernelProcessConfig`) and camel-cased constructors. Rely on total, pure functions where possible, and annotate tricky sections with brief Haddock comments. Document formatting choices in the same PR if you introduce new tooling.

## Testing Guidelines

Adopt `hspec` suites once runtime code lands; mirror the module tree under `test/` with files like `RequestRouterSpec.hs`. Run `cabal v2-test` before opening a PR and attach coverage or sample output when CI is unavailable. For interim prototypes, include deterministic repro steps (`scripts/dev-kernel.sh --json`) and expected responses in the PR description.

## Commit & Pull Request Guidelines

Write commit subjects in the imperative mood (`Add runtime manager sketch`) and keep bodies focused on rationale or follow-ups. Reference roadmap checklist items (see `docs/roadmap.md`) or related issues directly in the PR. Each PR should summarise behaviour changes, list any doc updates, and include screenshots or logs for user-visible work. Flag open questions or future tasks as Markdown checklists so reviewers can track them.

## Agent Workflow & Prompt Usage

Codex agents must follow the Specify toolkit prompts before running `/speckit` commands: review the matching files in `.codex/prompts/` (`speckit.specify.md`, `speckit.plan.md`, `speckit.tasks.md`, `speckit.implement.md`) and apply their checklists verbatim. Always invoke the helper scripts under `.specify/scripts/bash/` from the repo root with the documented flags, keep feature branches numbered (`001-name`), and update generated checklists when validation status changes.

## Active Technologies
- Haskell with GHC 9.12.2+ via ghcup + existing HsJupyter kernel, process, filepath, directory, unix (for system integration), optparse-applicative (CLI parsing) (004-install-cli)
- filesystem-based (Jupyter kernelspec directories, kernel.json files) (004-install-cli)
- Haskell (GHC 9.12.2+) + `zeromq4-haskell`, `aeson`, `katip`, `stm` (005-fix-jupyter-kernel-bug)
- N/A (in-memory kernel state) (005-fix-jupyter-kernel-bug)

- Haskell with GHC 9.12.2 via ghcup + hint >= 0.9.0 (GHC API), zeromq4-haskell, aeson, katip, stm (003-ghc-evaluation)
- In-memory interpreter state (hint InterpreterT monad) (003-ghc-evaluation)

- Haskell (GHC 9.6.4 via ghcup) + `zeromq4-haskell` for sockets, `aeson` for JSON, `bytestring`/`text`, `katip` for structured logging (001-protocol-bridge)
- Python helper script powered by `pyzmq` for local execute demos (001-protocol-bridge)
- N/A (in-memory runtime stub only) (001-protocol-bridge)

## Recent Changes

- 001-protocol-bridge: Added Haskell (GHC 9.6.4 via ghcup) + `zeromq4-haskell` for sockets, `aeson` for JSON, `bytestring`/`text`, `katip` for structured logging
