# HsJupyter

[![CI](https://github.com/jjunho/HsJupyter/workflows/CI/badge.svg)](https://github.com/jjunho/HsJupyter/actions/workflows/ci.yml)
[![Release](https://github.com/jjunho/HsJupyter/workflows/Release/badge.svg)](https://github.com/jjunho/HsJupyter/actions/workflows/release.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

HsJupyter is a next-generation Jupyter kernel for the Haskell programming language, implemented natively in Haskell. The project provides a fully functional GHC-powered runtime with persistent evaluation sessions, comprehensive resource management, and protocol-compliant Jupyter integration.

## Features

### Implemented ✅

- **Persistent GHC-powered execution engine** using the hint library for real Haskell evaluation
- **Full Jupyter protocol support** with ZeroMQ messaging, HMAC signature validation, and compliant envelope handling
- **Resource management** with configurable memory limits, CPU timeouts, and output truncation
- **Cancellation infrastructure** using STM-based tokens for graceful job interruption
- **Session state persistence** across cell executions with variable bindings and module imports
- **Comprehensive diagnostics** with syntax error detection and actionable suggestions
- **Structured logging and telemetry** using katip for observability
- **Constitutional compliance** following DRY, KISS, YAGNI, and SOLID principles

### In Development 🚧

- **CLI installation tools** for easy Jupyter kernel registration and management
- **Diagnostic utilities** (`doctor` command) for troubleshooting

### Planned 📋

- **Code completion** and inspection capabilities
- **Interactive debugging** with GHCi integration
- **Rich output renderers** for HTML, images, and interactive widgets
- **Package management** integration for notebook-specific dependencies

## Quickstart

### Prerequisites

- **GHC 9.6.7+** via ghcup: `ghcup install ghc 9.6.7 && ghcup install cabal`
- **ZeroMQ library**: `libzmq3-dev` (Debian/Ubuntu) or equivalent for your OS
- **Python 3.8+** (optional) with `pyzmq` for the demo client

### Building

Fast development build (5 seconds vs minutes):

```bash
cabal build lib:hs-jupyter-kernel -O0
```

Full optimized build:

```bash
cabal build
```

### Running the Kernel

Start the kernel server with a Jupyter connection file:

```bash
cabal run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Info
```

### Testing

Run targeted unit tests:

```bash
cabal test unit -O0 --test-option="--match=/GHCSession/"
```

Run all tests:

```bash
cabal test
```

Run integration tests with real GHC evaluation:

```bash
cabal test integration
```

### Demo: Python Client

Test the kernel without a full Jupyter installation using the demo client:

```bash
# Terminal 1: Start the kernel
cabal run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Debug

# Terminal 2: Send execute requests
python3 scripts/demo/phase1_echo_notebook.py \
  --connection scripts/demo/sample-connection.json
```

See `scripts/demo/README.md` for more details.

## Documentation

- [Architecture Overview](docs/architecture.md)
- [Roadmap](docs/roadmap.md)
- [Developer Guide](docs/developer/README.md)
- [Installation (WIP)](docs/installation/README.md)
- Protocol references: <https://jupyter-client.readthedocs.io/en/stable/>

Current prototype entrypoint: `app/KernelMain.hs`.

Run the echo demo against a Jupyter connection file:

```bash
cabal v2-run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Info
```

CLI flags come from `app/KernelMain.hs`:

- `--connection FILE` – path to the Jupyter connection file
- `--log-level (Debug|Info|Warn|Error)` – overrides `HSJUPYTER_LOG_LEVEL`

## Status

### Current Implementation (v0.1.0)

- ✅ **Phase 1: Protocol Bridge** - Complete ZeroMQ integration with Jupyter messaging protocol
- ✅ **Phase 2: Runtime Core** - STM-based job queue, session state, resource management
- ✅ **Phase 3: GHC Evaluation** - Real Haskell evaluation with hint library, persistent sessions
- 🔄 **Phase 4: Installation CLI** - In development (install, doctor, configuration commands)
- ⏳ **Phase 5: Advanced Features** - Planned (completions, debugging, package management)

### Key Modules Implemented

**Core Infrastructure:**

- `HsJupyter.KernelProcess` - Process lifecycle, socket management, main event loop
- `HsJupyter.Kernel.Types` - Shared type definitions and protocol types

**Protocol Layer:**

- `HsJupyter.Bridge.JupyterBridge` - ZeroMQ bridge with HMAC authentication
- `HsJupyter.Bridge.HeartbeatThread` - Heartbeat socket handling
- `HsJupyter.Bridge.Protocol.{Envelope,Codec}` - Message parsing and serialization

**Routing:**

- `HsJupyter.Router.RequestRouter` - Request dispatch and capability routing

**Runtime System:**

- `HsJupyter.Runtime.Manager` - Job queue and execution coordination
- `HsJupyter.Runtime.GHCSession` - Persistent interpreter state
- `HsJupyter.Runtime.GHCRuntime` - GHC evaluation engine with cancellation
- `HsJupyter.Runtime.Evaluation` - High-level evaluation interface
- `HsJupyter.Runtime.SessionState` - Binding and import persistence
- `HsJupyter.Runtime.Diagnostics` - Error reporting and suggestions
- `HsJupyter.Runtime.ErrorHandling` - Unified error handling patterns
- `HsJupyter.Runtime.GHCDiagnostics` - GHC-specific error analysis
- `HsJupyter.Runtime.ResourceGuard` - Resource limits and monitoring
- `HsJupyter.Runtime.Telemetry` - Metrics collection and reporting

**CLI Tools (In Development):**

- `HsJupyter.CLI.Commands` - Command-line interface parsing
- `HsJupyter.CLI.Types` - CLI data models
- `HsJupyter.CLI.Install` - Kernel installation logic
- `HsJupyter.CLI.Doctor` - Diagnostic utilities
- `HsJupyter.CLI.Configuration` - Configuration management
- `HsJupyter.CLI.Utilities` - CLI helper functions
- `HsJupyter.CLI.Output` - Output formatting

### Test Coverage

- **147+ test examples** across unit and integration test suites
- **Unit tests**: Protocol parsing, session state, diagnostics, resource management
- **Integration tests**: Full execute cycle, GHC evaluation, JSON output, CLI operations

## CLI Reference

Binary: `hs-jupyter-kernel` (entrypoint in `app/KernelMain.hs`)

Flags:

- `--connection FILE` — path to Jupyter connection JSON
- `--log-level Debug|Info|Warn|Error` — overrides env

Environment:

- `HSJUPYTER_LOG_LEVEL` — default log level when flag not provided

Structured logs use katip for observability. Exit messages are printed to stdout.

## Python Demo Client Details

Use the helper in `scripts/demo/` to exercise an `execute_request` roundtrip without a full Jupyter stack.

Prereq: `pip install pyzmq`

Steps:

- Terminal A: start the kernel

```bash
cabal run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Debug
```

- Terminal B: send an execute request

```bash
python3 scripts/demo/phase1_echo_notebook.py \
  --connection scripts/demo/sample-connection.json
```

The script sends Jupyter-compliant execute requests and prints the replies, demonstrating the full protocol roundtrip.

Excerpt (simplified) from `scripts/demo/phase1_echo_notebook.py` showing how the execute_request is built and signed:

```python
def build_execute_request(code: str, key: bytes) -> List[bytes]:
    header = {
        "msg_id": uuid.uuid4().hex,
        "session": uuid.uuid4().hex,
        "username": "demo",
        "msg_type": "execute_request",
        "version": "5.3",
        "date": time.strftime("%Y-%m-%dT%H:%M:%S"),
    }
    content = {"code": code, "silent": False, "store_history": True, "allow_stdin": False}
    encoded = [canonical_json(header), canonical_json({}), canonical_json({}), canonical_json(content)]
    signature = sign_frames(key, encoded)
    return [b"", signature.encode("utf-8"), *encoded]
```

This mirrors the Jupyter wire format used by the bridge (`Envelope` and `Codec` in `src/HsJupyter/Bridge/Protocol/`).

## Connection File Reference

The kernel reads a Jupyter connection JSON and binds ZeroMQ sockets accordingly. Minimal fields used by `app/KernelMain.hs` and `HsJupyter.KernelProcess`:

- `ip`: host to bind (e.g., "127.0.0.1")
- `transport`: scheme (e.g., "tcp")
- `shell_port`, `iopub_port`, `control_port`, `stdin_port`, `hb_port`: port numbers
- `signature_scheme`: "hmac-sha256" or empty for none
- `key`: HMAC key (string)

See a working sample in `scripts/demo/sample-connection.json`.

## Troubleshooting

### Ports already in use

Edit `scripts/demo/sample-connection.json` to change `shell_port`, `iopub_port`, etc., then pass it via `--connection`.

### Missing GHC/cabal

Install via ghcup:

```bash
ghcup install ghc 9.6.7
ghcup install cabal
```

Ensure they're on PATH.

### pyzmq not installed

```bash
pip install --user pyzmq
```

Or use a virtual environment.

### ZeroMQ library issues

Ensure libzmq is available on your system:

- Debian/Ubuntu: `sudo apt-get install libzmq3-dev`
- macOS: `brew install zeromq`
- Fedora: `sudo dnf install zeromq-devel`

### Slow builds

The hint library includes the full GHC API, resulting in long build times. For faster development:

```bash
cabal build lib:hs-jupyter-kernel -O0  # Fast unoptimized build
```

See `docs/developer/build-performance.md` for more optimization tips.

## Contributing

While the kernel is still under active design, we welcome feedback and discussion on the architecture. Please open an issue to share ideas or ask questions about the roadmap.
