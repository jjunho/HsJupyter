# HsJupyter Roadmap Tracker

This living checklist captures the near-term implementation tasks derived from the architecture plan. Update as milestones progress.

## In Progress

- [x] Prototype `KernelProcess` and `JupyterBridge` handshake (ZeroMQ message round-trip) - see `specs/001-protocol-bridge/plan.md` quickstart demo.
- [ ] Stand up persistent `RuntimeManager` with streaming stdout/stderr
- [ ] Design CLI skeleton for `hsjupyter install` (argument parsing, dry-run flow)

## Upcoming

- [ ] Implement artifact caching and environment detection in `EnvironmentService`
- [ ] Build `hsjupyter doctor` diagnostics CLI
- [ ] Wire completions/diagnostics capability providers behind feature flags
- [ ] Author benchmark suite scaffolding (`docs/performance`) and automate runs
- [ ] Populate installation and configuration guides with real procedures
- [ ] Draft support knowledge base articles for top installer/runtime issues

## Tracking

- Architecture reference: `docs/architecture.md`
- Status updates: add release plan details in `docs/releases/`
- Performance trends: record metrics in `docs/performance/`

> TODO: Once an issue tracker is available, mirror these items as issues/epics for richer tracking.

### Release Notes Snippet

- Phase 1 protocol bridge prototype available via `cabal v2-run hs-jupyter-kernel`.
- Echo runtime returns deterministic streams for demo validation.
- Guardrail tests validate HMAC signature checking ahead of ZeroMQ wiring.
