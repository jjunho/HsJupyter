# Implementation Plan: Phase 1 – Protocol Bridge

**Branch**: `001-protocol-bridge` | **Date**: 2025-10-24 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-protocol-bridge/spec.md`

## Summary

Phase 1 delivers a demonstrable Jupyter protocol bridge: `KernelProcess` boots from a connection file, `JupyterBridge` validates envelopes, and a stub runtime echoes `execute_request` traffic back to clients. The milestone proves the ZeroMQ handshake, signature checks, and observability hooks needed for roadmap progress.

## Technical Context

**Language/Version**: Haskell (GHC 9.6.4 via ghcup)
**Primary Dependencies**: `zeromq4-haskell` for sockets, `aeson` for JSON, `bytestring`/`text`, `katip` for structured logging
**Storage**: N/A (in-memory runtime stub only)
**Testing**: `hspec` with `hspec-wai` style helpers for protocol harness; nbclient smoke notebooks in CI
**Target Platform**: Linux and macOS developer workstations; CI containers
**Project Type**: Single backend service (kernel executable)
**Performance Goals**: Execute echo round-trip < 2s; heartbeat stability for 30 min soak
**Constraints**: Deterministic echo stub; deny unsigned envelopes; structured logs on every message
**Scale/Scope**: Single-kernel instance per notebook; designed for local development load

## Constitution Check

| Gate | Status | Notes |
|------|--------|-------|
| Documentation-first: spec and plan must precede implementation | ✅ | Spec drafted (`spec.md`), plan in progress. |
| Test-first mindset: define acceptance & soak tests before runtime work | ✅ | Echo acceptance scenarios captured in spec; soak metric in success criteria. |
| Observability baseline required in earliest phase | ✅ | Logging/metrics requirements listed in spec and plan summary. |

## Project Structure

### Documentation (this feature)

```text
specs/001-protocol-bridge/
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── contracts/
└── spec.md
```

### Source Code (repository root)

```text
app/
└── KernelMain.hs            # entry point wiring CLI + kernel bootstrap

src/HsJupyter/
├── KernelProcess.hs         # configuration, lifecycle supervision
├── Bridge/
│   ├── JupyterBridge.hs     # socket wiring, signature validation
│   └── Protocol/
│       ├── Envelope.hs      # typed message structures
│       └── Codec.hs         # JSON encode/decode helpers
├── Router/RequestRouter.hs  # dispatch execute/control frames
└── Runtime/EchoRuntime.hs   # stub runtime returning echoed results

scripts/
└── demo/phase1-echo.sh      # nbclient-driven echo harness

test/
├── unit/
│   ├── ProtocolEnvelopeSpec.hs
│   └── EchoRuntimeSpec.hs
└── integration/
    └── ExecuteEchoSpec.hs   # nbclient-driven round-trip
```

**Structure Decision**: Single executable kernel with modular `HsJupyter.*` namespace; scripts support manual demos; tests split into unit vs integration to mirror roadmap expectations.

## Complexity Tracking

No constitution violations identified; additional complexity log not required for this phase.

## Phase Breakdown

### Phase 0 – Research Focus

- Confirm Haskell ZeroMQ binding (`zeromq4-haskell`) suitability and thread-safety guarantees.
- Select structured logging library compatible with asynchronous workers.
- Define demo workflow using nbclient for automated echoes.

### Phase 1 – Design Deliverables

- Codify protocol data model (`ProtocolEnvelope`, channel enums, signature helpers).
- Document API contracts for execute echo and control interrupt flows.
- Capture manual quickstart for running the phase demo script against a Jupyter connection file.
- Update agent context with ZeroMQ/hspec learnings for future tasks.

### Phase 2 – Handoff to Tasks

- `/speckit.tasks` will map user stories to implementation tasks once design artifacts land.

## Constitution Re-Check

Design artifacts (research, data model, contracts, quickstart) align with documentation-first and observability gates. No new violations detected; proceed to tasks once `/speckit.tasks` is ready.
