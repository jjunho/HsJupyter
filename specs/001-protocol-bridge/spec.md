# Feature Specification: Phase 1 – Protocol Bridge

**Feature Branch**: `001-protocol-bridge`  
**Created**: 2025-10-24  
**Status**: Draft  
**Input**: User description: "Phase 1 – Protocol Bridge"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Complete Execute Echo (Priority: P1)

As a kernel maintainer I can boot the Phase 1 kernel and have an `execute_request` echoed to `execute_reply`/IOPub so stakeholders can see the protocol loop working end-to-end.

**Why this priority**: This is the minimum demonstrable value for the kernel; without it the project has no working artifact.

**Independent Test**: Use the documented Phase 1 demo command with a Jupyter connection file and observe an `execute_reply` with status `ok` plus streamed stdout in notebook logs.

**Acceptance Scenarios**:

1. **Given** the kernel starts with a valid connection file, **When** nbclient sends `print("ok")`, **Then** the bridge emits matching IOPub outputs and returns an execute_reply with status `ok` within 2 seconds.
2. **Given** the kernel is serving the control channel, **When** Jupyter issues an interrupt, **Then** the bridge acknowledges on control and the kernel process remains healthy.

---

### User Story 2 - Enforced Protocol Guardrails (Priority: P2)

As a security-conscious maintainer I need malformed or unsigned envelopes rejected so downstream components are never exposed to spoofed traffic.

**Why this priority**: Guardrails reduce protocol drift risk and align with the roadmap item on ZeroMQ handshake hardening.

**Independent Test**: Replay a captured message with a bad HMAC via the demo harness and confirm the kernel logs a signature failure without forwarding to the router.

**Acceptance Scenarios**:

1. **Given** an incoming message with an invalid HMAC, **When** the bridge validates headers, **Then** it drops the payload, emits a structured warning, and leaves sockets open for valid traffic.

---

### User Story 3 - Operability Insights (Priority: P3)

As an on-call maintainer I need baseline diagnostics so I can triage Phase 1 issues without attaching a debugger.

**Why this priority**: Early observability keeps the roadmap on track and prepares for CI smoke runs.

**Independent Test**: Run the kernel with debug logging enabled and verify correlation IDs, socket bindings, and heartbeat metrics appear in structured logs.

**Acceptance Scenarios**:

1. **Given** the kernel boots, **When** logging is enabled, **Then** startup logs include connection file path, socket endpoints, and capability flags.

---

### Edge Cases

- Connection file references sockets that fail to bind; kernel must exit gracefully with actionable error output.
- Heartbeat channel stops receiving probes; supervisor should log degraded state without crashing other channels.
- Execute payload exceeds 1 MB; bridge should stream without truncation and note payload size in metrics.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The kernel MUST parse Jupyter connection files and bind shell, control, stdin, iopub, and heartbeat sockets before accepting traffic.
- **FR-002**: The bridge MUST validate message signatures and ordering per Jupyter messaging spec, dropping and logging any failures.
- **FR-003**: The router MUST handle `execute_request` by forwarding to a stub runtime that echoes input code and emits matching `execute_result`/stream frames.
- **FR-004**: The kernel MUST surface structured logs with correlation IDs for every inbound and outbound message.
- **FR-005**: The control channel MUST honour interrupt and shutdown requests without terminating healthy workers.
- **FR-006**: Documentation MUST provide a quickstart showing how to launch the Phase 1 kernel against nbclient for manual verification.

### Key Entities *(include if feature involves data)*

- **KernelProcessConfig**: Captures CLI/config-derived settings (connection file path, log level, runtime mode) consumed during bootstrap.
- **ProtocolEnvelope**: Typed representation of Jupyter message frames (identities, header, parent, metadata, content) used for validation and logging.
- **ExecutionOutcome**: Stub runtime result containing echoed source, synthetic stream output, and status metadata returned to the bridge.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Demo script completes an execute echo round-trip in under 2 seconds on a developer laptop.
- **SC-002**: 100% of malformed or unsigned messages are rejected and logged without disturbing subsequent valid traffic during a 10-minute soak.
- **SC-003**: Heartbeat channel remains responsive (no missed acks) for 30 minutes under echo workload.
- **SC-004**: Onboarding instructions enable a new contributor to run the Phase 1 demo in under 15 minutes from a clean environment.

## Assumptions

- Phase 1 runtime remains a deterministic echo stub; evaluation of arbitrary Haskell code is deferred to Phase 2.
- Contributors install the recommended GHC 9.6.4 toolchain via ghcup as outlined in `AGENTS.md` before running demos.
- CI smoke coverage for Phase 1 will rely on nbclient-driven notebooks rather than full JupyterLab UI sessions.
