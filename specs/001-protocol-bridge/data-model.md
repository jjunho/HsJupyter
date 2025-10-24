# Data Model: Phase 1 – Protocol Bridge

## KernelProcessConfig

- **Fields**:
  - `connectionFile :: FilePath` — path to JSON file issued by Jupyter launcher.
  - `transport :: Text` — protocol (`tcp`, `ipc`).
  - `ip :: Text` — bind address (e.g., `127.0.0.1`).
  - `signatureScheme :: Text` — HMAC algorithm, defaults to `hmac-sha256`.
  - `key :: ByteString` — shared secret for message signing (may be empty for testing).
  - `shellPort`, `iopubPort`, `stdinPort`, `hbPort`, `controlPort :: Int` — channel ports.
  - `logLevel :: LogLevel` — resolved from CLI/env overrides.
- **Relationships**: Provides startup configuration consumed by `KernelProcess` and shared with `JupyterBridge`.
- **Validation Rules**: Ports must be >0; signature scheme required when key non-empty; file must exist and contain required fields.

## ProtocolEnvelope

- **Fields**:
  - `identities :: [ByteString]` — routing identities preserved on replies.
  - `header :: MessageHeader` — typed header (message ID, username, session, msgType, version).
  - `parentHeader :: Maybe MessageHeader` — parent message for replies.
  - `metadata :: Value` — JSON metadata forwarded untouched.
  - `content :: Value` — typed content decoded per message type.
  - `signature :: ByteString` — HMAC digest verifying `header` through `content` frames.
- **Relationships**: Instances produced by `JupyterBridge`, consumed by `RequestRouter`, and re-encoded on outbound frames.
- **Validation Rules**: Signature must match computed digest; message order recorded in bridge logs; reject envelopes missing header fields.

## ExecutionOutcome

- **Fields**:
  - `status :: ExecuteStatus` — `Ok` or `Error` for Phase 1 stub.
  - `payload :: Value` — JSON payload for execute result (echoed input code).
  - `streams :: [StreamChunk]` — stdout/stderr frames mirrored to IOPub.
  - `executionCount :: Int` — incremented per execute request.
- **Relationships**: Produced by `EchoRuntime`, translated into protocol frames by `RequestRouter`.
- **Validation Rules**: Execution count must monotonically increase; payload echoes original source for traceability; stub emits deterministic stream entries.

## Auxiliary Types

- **MessageHeader**: includes `msgId`, `session`, `username`, `date`, `msgType`. Ensures RFC3339 timestamp parsing.
- **Channel**: enum (`Shell`, `IOPub`, `Control`, `Stdin`, `Heartbeat`) guiding socket routing.
- **HeartbeatStatus**: captures last probe timestamp and rolling latency to feed observability metrics.

## State Transitions

- `KernelProcess` lifecycle: `Uninitialised → BindingSockets → Running → Draining → Terminated` with graceful shutdown on control messages.
- `HeartbeatStatus`: `Healthy` (latency <= 500ms) ↔ `Degraded` (latency > 500ms) → `Unresponsive` (> 5s, triggers warning log).

## Data Volume & Scale Assumptions

- Messages limited to < 1 MB per spec; backlog processed sequentially per channel.
- In-memory structures only; no persistence required in Phase 1.
