# HsJupyter Architecture Overview

This document outlines a lean, modular architecture for a high-performance Haskell Jupyter kernel. The design emphasises DRY, KISS, and YAGNI principles while keeping room for future scalability.

## High-Level Flow

1. `KernelProcess` boots, loads configuration, and wires logging/metrics. See `src/HsJupyter/KernelProcess.hs` and CLI in `app/KernelMain.hs`.
2. `JupyterBridge` binds ZeroMQ sockets, validates envelopes, and produces typed protocol events. See `src/HsJupyter/Bridge/JupyterBridge.hs` plus `Protocol.{Envelope,Codec}`.
3. `RequestRouter` dispatches each event to a capability handler (execute, complete, inspect, etc.). Current scaffold at `src/HsJupyter/Router/RequestRouter.hs`.
4. `RuntimeManager` evaluates Haskell code through a persistent GHC session and streams outputs. See `src/HsJupyter/Runtime/{Manager,GHCSession,GHCRuntime,Evaluation}.hs`.
5. Results flow back through `JupyterBridge` as structured Jupyter messages.

The same pipeline handles control messages (interrupt, shutdown) and capability-specific flows such as completions and widgets.

## Jupyter Protocol References

- Jupyter messaging specification: <https://jupyter-client.readthedocs.io/en/stable/messaging.html>
- Kernel connection files and channels: <https://jupyter-client.readthedocs.io/en/stable/kernels.html>
- Kernel provisioning overview: <https://jupyter-client.readthedocs.io/en/stable/provisioning.html>
- Upstream changelog and release notes for `jupyter_client`: <https://github.com/jupyter/jupyter_client/releases>
- Contribution and security policies for upstream client: <https://github.com/jupyter/jupyter_client/blob/main/CONTRIBUTING.md>, <https://github.com/jupyter/jupyter_client/blob/main/SECURITY.md>

## Core Components

### KernelProcess

- Parses CLI arguments, resolves configuration (`HsJupyter.Config`).
- Starts structured logging (e.g. katip/co-log) with per-message correlation IDs.
- Supervises worker threads and ensures graceful teardown on failure.

### JupyterBridge

- Owns ZeroMQ sockets for shell, iopub, control, stdin channels.
- Prototype module lives at `src/HsJupyter/Bridge/JupyterBridge.hs`, binding ZeroMQ sockets, verifying HMAC signatures, and translating multipart frames into typed envelopes for the Phase 1 echo runtime.
- Provides codec layer via `src/HsJupyter/Bridge/Protocol/{Envelope,Codec}.hs` with typed message records, avoiding ad-hoc JSON handling.
- Validates signatures, message order, and routing metadata before passing requests to the router.
- Serialises replies/outputs from typed data back into Jupyter wire format.

### RequestRouter

- Routes decoded events to capability handlers registered in a `CapabilityRegistry`.
- Keeps concerns separated: execution, completions, inspections, diagnostics, widgets.
- Normalises cancellation, deadlines, and error propagation.

### RuntimeManager

- Wraps a long-lived GHC session (via `hint`), implemented across `GHCSession`, `GHCRuntime`, and `Evaluation` modules.
- Compiles cells incrementally, tracks module state in `DynamicScope`, and exposes soft resets.
- Streams stdout/stderr via callbacks; collects rich results into `ExecutionOutcome`.
- Enforces resource limits in cooperation with `RuntimeSupervisor`.

### RuntimeSupervisor & JobQueue

- Maintains a bounded `TBQueue` of `CellJob` items to prevent overload.
- Runs jobs in async workers, tagging them with parent message IDs to simplify cancellation.
- Emits runtime metrics (latency, queue depth, memory) for observability.

## Supporting Services

### EnvironmentService

- Detects Stack/Cabal projects or bare GHC environments.
- Creates hashed sandboxes in `~/.cache/hsjupyter/<ghc-version>/<hash>` to reuse compiled artifacts.
- Prefers prebuilt runtime bundles shipped with releases; falls back to building from source only when custom dependencies are requested.
- Exposes per-notebook dependency resolution while keeping execution layer agnostic of tooling specifics.

### Capability Providers

- `CompletionProvider`: integrates with HLS APIs or `ghci :complete`, caches symbol tables per scope.
- `DiagnosticsProvider`: maps compilation/runtime errors into structured diagnostics with source spans.
- `WidgetServer`: manages comm targets and widget state, synchronises frontend/back-end lifecycle events.
- `ResourceMonitor`: samples CPU/memory usage, can issue warnings or throttle execution.

### Persistence & Caching

- `SessionStore` (SQLite or JSON) persists notebook metadata, environment hashes, widget state.
- `ArtifactCache` stores compiled modules/object files for faster cold starts.
- Both stores remain optional to keep stateless deployments straightforward.

## Cross-Cutting Concerns

- Config managed via lightweight `ReaderT Env` stack; services depend on minimal typeclasses (`MonadRuntime`, `MonadBridge`) to keep modules testable.
- Structured logging and metrics instrumentation at boundaries (bridge, router, runtime).
- Error handling centralised in `ExecutionOutcome` to translate exceptions into user-friendly diagnostics.

## Testing Strategy

- Unit tests for protocol codecs, renderers, and environment hashing.
- Property tests verifying message round-trips and cache determinism.
- Integration tests driven by `nbclient` executing golden notebooks and asserting message sequences.
- Load tests to validate supervisor throttling under concurrent execution.

## Implementation Milestones

1. **Bootstrap**: skeleton Cabal project, placeholder modules, build pipeline (CI + formatter + hlint).
2. **Bridge Prototype**: implement `KernelProcess` + `JupyterBridge` loop; round-trip `execute_request` returning a fixed reply.
3. **Runtime Integration**: hook in `RuntimeManager`, stream stdout/stderr, map errors to diagnostics.
4. **Capabilities**: add completions, inspections, widgets behind feature flags to avoid premature complexity.
5. **Performance Hardening**: enable artifact caching, introduce monitoring endpoints, run load tests.
6. **Distribution Pipeline**: automate building signed binaries, packaging precompiled dependency bundles, and running the bootstrap installer end-to-end in CI.

## Installation & Distribution

- **Prebuilt releases**: publish statically linked binaries (one per major platform) bundled with the kernel spec so users run a single installer without compiling dependencies.
- **Curated package set**: ship a frozen snapshot of core libraries (`.cabal.project.freeze` or Stack snapshot) and reuse precompiled artifacts stored in release assets to avoid repeated package builds.
- **Bootstrap script**: provide a lightweight installer (`hsjupyter install`) that detects existing GHC (via GHCup), downloads the matching runtime bundle, installs the kernel spec, and verifies health—no manual Stack/Cabal steps.
- **Portable archive**: offer a zip/tarball containing the binary, kernel spec, and default config for offline installs; include hooks so users can override the runtime path if they already have GHC.
- **Container & Nix paths**: maintain Docker images and a Nix flake for reproducibility, but keep them optional so the simplest path is still a single download.

## User Experience Goals

- First-run target: from download to executing a notebook cell in <5 minutes on a clean machine.
- Zero-compilation experience for default installation paths; prebuilt artifacts must cover mainstream platforms (Windows/macOS/Linux).
- Installer outputs actionable guidance (e.g. missing Jupyter) with copy-paste fixes and retry instructions.
- Upgrades preserve user kernels and cached environments without manual cleanup.

## Bootstrap Installer Blueprint

1. Detect prerequisites: check for Jupyter, Python, and GHC installations; offer to install or point to documentation if missing.
2. Select runtime bundle: choose the prebuilt archive matching OS + GHC version; download with checksum validation.
3. Lay down files: extract binary, config, and kernel spec into user-level directories (`~/.local/share/jupyter/kernels/hsjupyter`).
4. Register cache: populate artifact cache directory and record metadata for future updates.
5. Health check: run a smoke test notebook via `nbclient` to confirm execution path.
6. Telemetry (opt-in): optionally report anonymous install success/failure to improve bundles.
7. Provide uninstall command that cleans binaries, kernel spec, caches, and keeps user notebooks untouched.

### Installer UX Flow

- Entry: `hsjupyter install` displays summary of actions, asks for consent, and supports `--yes` for automation.
- Prerequisite resolver prints detected versions; missing tools accompanied by direct download commands.
- Progress output uses concise status lines (`[1/5] Downloading runtime bundle…`), with retry logic and checksum verification feedback.
- Post-install summary includes kernel spec path, cache location, and command to launch JupyterLab; exposes `--diagnose` flag to run extra checks.
- Uninstall path mirrors UX with `hsjupyter uninstall`, providing dry-run diff before deleting files.
- Internationalisation kept simple: structured messages ready for future translation without hard-coding text in CLI logic.

## Release Workflow

- Versioning: semantic versioning with synchronized tags across binary, runtime bundles, and installer.
- Continuous integration: matrix builds on CI (Linux/macOS/Windows) producing signed binaries, runtime bundles, and checksum manifests.
- Quality gates: automated smoke notebooks, lint/test suites, and installer E2E tests within CI before release promotion.
- Distribution: publish assets to GitHub Releases plus a lightweight CDN for bootstrap downloads; update hash manifest consumed by the installer.
- Documentation cadence: release notes summarizing features/fixes, upgrade instructions, and compatibility matrix.

## Maintenance & Support

- Long-term support branches for the latest two minor versions, receiving security fixes and critical bug patches.
- Scheduled GHC compatibility reviews (e.g. quarterly) with decision logs for dropping old compilers.
- Bug triage rotation with response-time targets; template reproductions to keep issue reports actionable.
- Diagnostics command (`hsjupyter doctor`) collecting environment info and recent logs to simplify support workflows.

### `hsjupyter doctor` Specification

- Collects versions (HsJupyter, GHC, Python, Jupyter, OS) and prints them in a machine-readable table.
- Validates runtime bundle integrity (checksum, timestamp) and reports missing artifacts.
- Runs optional notebook smoke test with `--run-smoke`; provides guidance if execution fails.
- Scrubs sensitive data (home paths, environment secrets) before saving optional support bundle (zip of logs + summary).
- Exit codes: `0` for healthy, `10` for warnings (e.g. outdated bundle), `20` for failures (missing dependencies).
- Integrates with bootstrap installer so users can run diagnosis immediately after failed installs.

## Deployment & Ops

- Distribute update notifications via the bootstrap tool; allow delta updates so users avoid re-downloading large packages.
- Ship Docker images bundling JupyterLab + HsJupyter for zero-config demos.
- Expose metrics (EKG/Prometheus) and structured logs to integrate with user observability stacks.

## Risk & Mitigation

- **Slow installs**: mitigate with prebuilt artifacts and delta updates; monitor installer telemetry for regressions.
- **Binary incompatibilities**: incorporate nightly smoke runs across supported OS/GHC combinations; rollback plan via manifest pinning.
- **Protocol drift**: track Jupyter kernel spec updates in automated checks; fail builds when schema diverges.
- **Resource exhaustion**: enforce supervisor limits and provide configuration knobs for memory/timeouts; document tuning recipes.

## Configuration & Customisation

- Configuration hierarchy: default config baked into binary → system-wide overrides (e.g. `/etc/hsjupyter/config.toml`) → user config (`~/.config/hsjupyter/config.toml`) → per-notebook metadata.
- Configured via TOML with schema validation; environment variables offer quick overrides (`HSJUPYTER_LOG_LEVEL`, `HSJUPYTER_RUNTIME_PATH`).
- Kernel spec metadata exposes toggleable capabilities (widgets, completions) without editing config files.
- Provide `hsjupyter config edit` helper to open user config in editor with comments explaining options.
- Document safe defaults for resource limits, telemetry, and diagnostics to ensure predictable behaviour.

## Roadmap Timeline (Indicative)

- **Phase 0 – Foundations (Weeks 1-2)**: set up repository, CI, coding standards, scaffolding for core modules, initial architecture diagrams.
- **Phase 1 – Protocol Bridge (Weeks 3-4)**: implement `KernelProcess`, `JupyterBridge`, typed protocol models, and minimal execute echo path; deliver first integration demo.
- **Phase 2 – Runtime Core (Weeks 5-7)**: integrate persistent GHC session, streaming outputs, cancellation, and baseline diagnostics; run real notebooks.
- **Phase 3 – Capabilities & UX (Weeks 8-10)**: add completions, inspections, widget support, logging/metrics polish; introduce `hsjupyter doctor`.
- **Phase 4 – Distribution (Weeks 11-12)**: build bootstrap installer, package prebuilt artifacts, exercise E2E install tests across platforms.
- **Phase 5 – Hardening & Release (Weeks 13-14)**: performance profiling, load testing, documentation finalisation, publish release candidates, gather beta feedback.

## CI & Release Automation Blueprint

- CI stages: `lint` → `unit-tests` → `integration-tests` → `build-binaries` → `package-bundles` → `installer-e2e`.
- Use GitHub Actions workflows with reusable jobs; caches GHC toolchains via GHCup actions; caches cabal/store directories keyed by snapshot hash.
- Binary signing performed in dedicated job using hardware-backed secrets (e.g. cosign or codesign on macOS); artifacts notarised where required.
- Nightly builds on `main` channel update a `nightly` manifest consumed by optional bleeding-edge installers.
- Release promotion pipeline triggered by tagging: runs full suite plus manual approval gate before publishing artifacts and updating release notes.
- Post-release job pushes documentation updates and notifies bootstrap manifest CDN to ensure new hashes propagate.

## Telemetry & Privacy

- Installer telemetry opt-in by default off; prompt explains data collected (anonymised OS, install success/failure, durations).
- Runtime telemetry focuses on anonymised metrics (latency, errors) using hashed identifiers; supports `--no-telemetry` flag and config toggle.
- Data retention policy: aggregate metrics retained for 90 days; raw logs discarded unless user provides support bundle.
- Publish privacy statement and data schema; include `hsjupyter telemetry status` command for transparency.
- Ensure telemetry paths work offline by queuing events locally and discarding after expiry—no failures when network unavailable.

## Community & Contribution

- CONTRIBUTING guide outlines coding standards, review expectations, and CLA (if any).
- Decision log (`docs/decisions/ADR-XXXX.md`) documents major architectural choices to keep contributors aligned.
- Governance model: maintainers group with documented release permissions; onboarding checklist for new maintainers.
- Encourage community kernels/extensions via capability interfaces; provide examples in `docs/examples/community`.
- Regular community sync (monthly) to review roadmap, gather feedback, and highlight contributors.

## Support Operations

- Tiered support model: community support via GitHub Discussions, issue tracker triaged by maintainers, escalations handled through dedicated email/contact.
- SLA targets: community responses within 48 hours, critical bugs acknowledged within 24 hours, patches scoped within 5 business days when feasible.
- Support playbooks stored in `docs/support/` covering common install issues, runtime diagnostics, and workarounds; kept in sync with `hsjupyter doctor` outputs.
- Incident response checklist for outages (e.g. broken installer manifest) including rollback steps, communication templates, and post-mortem workflow.
- Knowledge base curated from repeated issues, linked in installer/doctor messages to deflect known pain points.

## Security & Compliance

- Threat modelling: identify attack surfaces (ZeroMQ sockets, notebook inputs, installer downloads) and document mitigations.
- Code execution sandboxing: run user code in controlled OS processes with resource limits; optional seccomp profile for Linux containers.
- Signed release assets with reproducible builds; verify signatures in bootstrap installer before unpacking.
- Handle secrets carefully: redact environment variables in logs, require explicit opt-in to expose credentials.
- Security response plan: dedicated contact, 90-day disclosure policy, and CVE issuance process where applicable.
- Dependency scanning (Cabal audit, GitHub Dependabot/Nix advisories) integrated into CI; failing builds block releases until patched.

## Observability Strategy

- Logging: structured JSON logs with correlation IDs; log levels configurable per component; default redaction of sensitive data.
- Metrics: expose Prometheus-compatible endpoint with counters (executions, errors), histograms (latency, compile time), gauges (memory usage).
- Tracing: optional OpenTelemetry integration capturing execution spans (bridge receive → runtime evaluate → bridge send) for deep debugging.
- Dashboard templates provided (Grafana, CloudWatch) so operators can import observability setup quickly.
- Alerting recommendations: thresholds for queue depth, execute latency, installer failure rate; integrate with PagerDuty/Opsgenie if desired.
- Provide `hsjupyter observe collect` command to bundle recent logs/metrics snapshots for support scenarios.

## Performance Targets

- Cold start (first cell) < 6 seconds on mainstream laptops; warm cell execution < 300 ms for simple expressions.
- Installer runtime < 2 minutes on broadband connections with cached bundles; < 10 minutes on fresh download.
- Memory footprint: idle kernel < 200 MB, active notebook < 600 MB; document tuning options.
- Concurrency: handle at least 4 parallel execute requests with predictable latency; degrade gracefully beyond that using queue backpressure.
- Widget responsiveness: UI updates propagate within 200 ms round-trip under normal load.
- Publish benchmark suite measuring these targets; run in CI to track regressions.

### Benchmarking Plan

- Tooling: use Criterion for microbenchmarks (expression evaluation), custom notebook harness for macro metrics, and hyperfine for installer timing.
- Scenarios: cold start (fresh cache), warm start (cache primed), heavy notebook (data/plotting), concurrent cells, widget interactions.
- Environment: define reference hardware/software profiles; run on CI plus dedicated performance machines for consistency.
- Reporting: store results in `docs/performance/benchmarks.json` with historical comparisons; flag regressions above defined thresholds.
- Automation: nightly performance pipeline executes benchmark suite, publishes trend charts, and creates alerts for significant deviations.

## Testing Matrix

- Platforms: Windows (x64/ARM64), macOS (Intel/Apple Silicon), Linux (Ubuntu LTS, Fedora).
- GHC versions: latest LTS, previous minor, and Long-Term Support release; document unsupported combinations.
- Jupyter environments: classic Notebook, JupyterLab, VS Code notebooks; smoke tested via automation.
- Feature paths: execution, completions, widgets, diagnostics, installer install/uninstall, doctor tool.
- Accessibility tests: verify screen readers on macOS VoiceOver and NVDA, contrast checks for outputs.
- Track matrix coverage via CI dashboard with pass/fail history and flaky test alerts.

## Documentation Roadmap

- `README`: concise overview, quickstart, and link to installer instructions.
- `docs/architecture.md`: living document updated every milestone; add diagrams (component, sequence, deployment).
- `docs/installation/*`: platform-specific guides, troubleshooting, and FAQ.
- `docs/reference/configuration.md`: detailed config options with examples and schema definitions.
- `docs/developer/*`: contribution guide, coding standards, ADR index, testing walkthroughs.
- `docs/releases/*`: changelog, upgrade notes, compatibility matrix; generated during release pipeline.
- Plan documentation sprints aligned with major milestones to ensure docs ship in lockstep with features.

## Support Knowledge Base

- Structure topics by lifecycle: Installation, Runtime, Widgets, Environment, Troubleshooting, FAQ.
- Each article template includes symptoms, root cause, quick fix, long-term mitigation, and links to related docs.
- Integrate with `hsjupyter doctor` outputs (error codes map to KB articles) and installer guidance.
- Provide community contribution process for KB entries (review checklist, approval workflow).
- Host KB in `docs/support/kb` with static site generator (e.g., mkdocs) for searchable web docs.

## Accessibility Roadmap

- Conduct accessibility audit alongside Phase 3 to identify gaps in Jupyter integration, messaging clarity, and keyboard navigation.
- Ensure CLI tools (`install`, `doctor`) support screen readers (aria-friendly output) and high-contrast terminal themes.
- Provide alternative text for generated visual outputs and expose hooks for tactile/sonification tools where possible.
- Collaborate with accessibility testers to validate widget interactions; document best practices for notebook authors.
- Publish accessibility statement outlining compliance targets (WCAG 2.1 AA) and progress, updated each release.

## Localization Strategy

- Internationalise CLI messages by storing strings in locale files (e.g., JSON or gettext); default English, community-driven translations.
- Allow kernel metadata (display name, help text) to be localized via config.
- Documentation translation plan: prioritise quickstart and troubleshooting pages; leverage community translation sprints with review process.
- Support locale detection from environment variables while allowing `--locale` override for CLI tools.
- Ensure telemetry and diagnostics handle Unicode paths and messages reliably.

## Licensing & Compliance

- Project licensed under Apache 2.0 (or preferred permissive license); include SPDX headers in source files.
- Dependency license scanning as part of CI, generating attribution reports for release artifacts.
- Provide third-party notice file bundled with binaries; include guidance for commercial usage.
- Document export compliance stance (e.g., no encryption beyond standard HTTPS) and confirm compatibility with target regions.
- Maintain contributor license agreements or Developer Certificate of Origin workflow aligned with governance model.

## Ethical & Legal Considerations

- Clearly communicate that notebook code executes locally and retains user responsibility; highlight risks around executing untrusted notebooks.
- Provide guidelines for handling sensitive data in notebooks and discourage storing credentials in plain text cells.
- Respect user privacy by keeping telemetry optional and anonymised; document data usage policies prominently.
- Ensure documentation and community guidelines foster inclusive language and respectful collaboration.
- Review third-party dependencies for known ethical concerns (e.g., licensing conflicts, vulnerable packages) during dependency audits.

## Sustainability & Funding

- Outline potential funding models (donations, sponsorships, grants) to support maintenance and infrastructure costs.
- Maintain transparent expense reports if funds are managed; allocate budget for CI runners, hosting, and accessibility testing.
- Encourage contributor growth through mentorship programs and pairing sessions; track contributor retention metrics.
- Explore partnerships with academic or industrial groups using Haskell notebooks to share maintenance load.
- Periodically reassess project scope against available resources, pruning non-essential features to avoid maintainer burnout.

## Integration Roadmap

- Identify key integrations: JupyterLab extensions, nbconvert exporters, Haskell language server adapters, data viz libraries (VegaLite, Chart).
- Plan progressive integration milestones (e.g., basic HLS integration in Phase 3, advanced tooling post-launch).
- Document APIs/hooks for third-party tools to extend the kernel (custom display renderers, notebook magics).
- Collaborate with upstream Jupyter and Haskell ecosystem projects to align roadmaps and avoid duplication.
- Track integration requests from community feedback, prioritising those with broad impact.

## Release Communication

- Pre-release: publish beta notes, invite testers via newsletter/social channels; highlight testing focus areas.
- Launch: coordinated announcement (blog, mailing list, social posts) with clear upgrade instructions and headline features.
- Post-release: collect feedback via surveys, track adoption metrics, and publish follow-up posts if hotfixes required.
- Maintain public roadmap board showing upcoming milestones and status to keep community engaged.
- Document communication templates in `docs/releases/templates` for consistency across releases.

## Open Questions & Future Work

- Choose between `hint` and direct GHC API for runtime—prototype both to compare performance and complexity.
- Evaluate feasibility of WebAssembly compilation for running Haskell code in browser-backed Jupyter clients.
- Investigate distributed execution (remote runtimes, Kubernetes integration) once single-host path stabilises.
- Determine long-term packaging strategy for Windows ARM and Apple Silicon universal binaries.
- Plan accessibility features (screen-reader friendly outputs, high-contrast themes) for notebook interactions.

## Next Steps

- Finalise interfaces for core modules (`HsJupyter.Bridge`, `HsJupyter.Runtime`, `HsJupyter.Capabilities`).
- Prototype bridge ↔ runtime handshake to validate ZeroMQ framing and GHC session management.
- Iterate documentation as implementation details solidify, keeping diagrams and sequence charts in sync with the code.
