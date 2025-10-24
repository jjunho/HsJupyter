# Research Summary: Phase 1 – Protocol Bridge

## Decision 1: ZeroMQ Binding

- **Decision**: Use `zeromq4-haskell` for socket management.
- **Rationale**: Mature binding exposing DEALER/ROUTER patterns, proven in prior kernels, supports CURVE/HMAC, and integrates with `async` for supervised threads.
- **Alternatives considered**: `hszmq` (older API, limited maintenance), FFI shim to libzmq via custom bindings (higher effort, less type safety).

## Decision 2: Structured Logging Stack

- **Decision**: Adopt `katip` for structured logging and metrics hooks.
- **Rationale**: Provides JSON logging with namespaces, supports thread-local contexts, aligns with architecture plan calling out katip/co-log, and offers sinks for stdout plus file rotation.
- **Alternatives considered**: `co-log` (lighter but less opinionated about structure), manual `aeson` logging (higher maintenance, no rotation helpers).

## Decision 3: Demo & Test Harness

- **Decision**: Drive acceptance tests with `nbclient` executing a golden notebook through the kernel.
- **Rationale**: nbclient matches the spec’s acceptance scenario, runs headless in CI, and exercises the full message loop required for Phase 1 success criteria.
- **Alternatives considered**: Custom Python harness using `jupyter_client` (more boilerplate), manual JupyterLab demos (not automatable, breaks doc-first mandate).

## Decision 4: Heartbeat Monitoring Strategy

- **Decision**: Implement heartbeat responder using a dedicated lightweight thread that logs latency metrics via `katip` gauges.
- **Rationale**: Keeps heartbeat responsiveness independent from execute routing, supports the success criterion for 30-minute stability, and surfaces diagnostics for on-call maintainers.
- **Alternatives considered**: Multiplex heartbeat on main bridge loop (risk of starvation under load), defer heartbeat metrics to later phases (conflicts with observability gate).

## Decision 5: Configuration Loading

- **Decision**: Parse connection files via `aeson` into `KernelProcessConfig` with validation guarding missing fields.
- **Rationale**: Ensures predictable bootstrap behaviour, allows extension for CLI overrides, and matches requirement to bind sockets before traffic.
- **Alternatives considered**: Shell-out to Python to parse JSON (introduces runtime dependency), hand-rolled parser (reinvents `aeson` decoding and complicates schema updates).
