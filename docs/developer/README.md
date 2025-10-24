# Developer Guide (Draft)

This guide will help contributors understand the project structure, development workflows, and quality standards once coding begins.

## Planned Contents

1. **Getting Started**
   - Repository layout overview
   - Toolchain requirements (GHC, cabal/stack, Python for tests)
2. **Development Workflow**
   - Branching and commit guidelines
   - Code formatting and linting
   - Running unit, integration, and benchmark suites
3. **Architecture Primer**
   - Pointers to `docs/architecture.md`
   - Key modules and ownership boundaries
4. **Testing Guidance**
   - How to add new tests
   - Recording benchmarks and interpreting results
5. **Contribution Process**
   - Opening issues and feature proposals
   - Review expectations
   - Release checklist for maintainers

> TODO: Replace this outline with concrete instructions as the implementation and tooling solidify.

## Phase 1 Kernel Prototype Notes

- Run `cabal v2-run hs-jupyter-kernel -- --connection scripts/demo/sample-connection.json --log-level Debug` to bind the ROUTER/PUB/REP sockets for the echo prototype.
- Set `HSJUPYTER_LOG_LEVEL` to override the default log level when CLI flags are omitted.
- `scripts/demo/phase1_echo_notebook.py` now uses pyzmq to send a signed `execute_request`, prints the resulting `execute_reply`, and checks the heartbeat channel.
