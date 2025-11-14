# HsJupyter

[![CI](https://github.com/jjunho/HsJupyter/workflows/CI/badge.svg)](https://github.com/jjunho/HsJupyter/actions/workflows/ci.yml)
[![Release](https://github.com/jjunho/HsJupyter/workflows/Release/badge.svg)](https://github.com/jjunho/HsJupyter/actions/workflows/release.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

HsJupyter is a next-generation Jupyter kernel for the Haskell programming language with a comprehensive CLI for installation and management. Built natively in Haskell with a persistent GHC-powered runtime, comprehensive resource management, and protocol-compliant Jupyter integration.

## Quick Start

### Installation

Install the HsJupyter kernel with a single command:

```bash
hs-jupyter-kernel install --user
```

### Verify Installation

```bash
# Check installation status
hs-jupyter-kernel list

# Run comprehensive diagnostics
hs-jupyter-kernel doctor
```

### Start Using

Launch Jupyter and select the Haskell kernel:

```bash
jupyter notebook
# or
jupyter lab
```

## CLI Commands

HsJupyter provides a complete CLI for kernel management:

### `install` - Install the Haskell Kernel

```bash
# User installation (recommended)
hs-jupyter-kernel install --user

# System-wide installation
hs-jupyter-kernel install --system

# Custom configuration
hs-jupyter-kernel install --display-name "Haskell Custom" --memory-limit 2048

# Force reinstallation
hs-jupyter-kernel install --force
```

**Key Features:**
- Automatic Jupyter environment detection
- Custom GHC path support (`--ghc-path`)
- Resource limit configuration (`--memory-limit`, `--exec-timeout`)
- Installation validation (none, basic, full)
- JSON output for automation (`--json`)

### `doctor` - Diagnose Installation Issues

```bash
# Run comprehensive diagnostics
hs-jupyter-kernel doctor

# Check specific components
hs-jupyter-kernel doctor --check jupyter
hs-jupyter-kernel doctor --check kernel
hs-jupyter-kernel doctor --check ghc

# Save diagnostic report
hs-jupyter-kernel doctor --report diagnostic-report.json
```

**Provides:**
- Health checks for Jupyter, Kernel, GHC, and System
- Issue severity classification (Critical, Major, Minor, Warning)
- Actionable recommendations with specific commands
- System information collection

### `list` - List Installed Kernels

```bash
# List functional installations
hs-jupyter-kernel list

# Show all installations (including broken)
hs-jupyter-kernel list --all

# JSON output for scripting
hs-jupyter-kernel list --json
```

### `uninstall` - Remove Kernel

```bash
# Interactive uninstall with confirmation
hs-jupyter-kernel uninstall

# Skip confirmation prompt
hs-jupyter-kernel uninstall --confirm
```

### `version` - Version Information

```bash
# Display version information
hs-jupyter-kernel version

# JSON output
hs-jupyter-kernel version --json
```

## Features

### Implemented ✅

- **Simple Installation**: One-command setup with automatic environment detection
- **Comprehensive Diagnostics**: Built-in troubleshooting with actionable recommendations
- **Custom Configuration**: Resource limits, custom paths, display names, environment variables
- **Automation-Friendly**: JSON output mode for CI/CD integration
- **Cross-Platform**: Linux, macOS, and Windows support
- **Persistent GHC-powered execution**: Real Haskell evaluation with hint library
- **Full Jupyter protocol support**: ZeroMQ messaging, HMAC signature validation
- **Resource management**: Configurable memory limits, CPU timeouts, output truncation
- **Session state persistence**: Variable bindings and module imports across cells
- **Structured logging**: katip-based observability
- **Constitutional compliance**: SOLID principles, comprehensive error handling

### In Development 🚧

- Code completion and inspection capabilities
- Interactive debugging with GHCi integration

### Planned 📋

- Rich output renderers for HTML, images, and interactive widgets
- Package management integration for notebook-specific dependencies

## Performance

- **Installation**: <30 seconds (with dependencies available)
- **Diagnostics**: <1 second
- **Memory Usage**: <100MB during operations
- **Kernel Startup**: <5 seconds

## Documentation

- [Installation Guide](docs/migration-from-install-script.md) - Complete installation and migration guide
- [Architecture Overview](docs/architecture.md) - System design and components
- [CHANGELOG](CHANGELOG.md) - Version history and feature releases
- [Developer Guide](docs/developer/README.md) - Contributing and development workflow
- [Roadmap](docs/roadmap.md) - Future plans and milestones
- Jupyter protocol references: <https://jupyter-client.readthedocs.io/en/stable/>

## Advanced Usage

### Custom Configuration Files

Create a configuration file for complex setups:

```json
{
  "display_name": "Haskell (Custom)",
  "ghc_path": "/custom/path/to/ghc",
  "resource_limits": {
    "memory_mb": 2048,
    "timeout_seconds": 300,
    "max_output_kb": 1024
  },
  "environment": {
    "CUSTOM_VAR": "value"
  }
}
```

Install using the configuration:

```bash
hs-jupyter-kernel install --config my-config.json
```

### CI/CD Integration

```bash
# Install in CI pipeline
hs-jupyter-kernel install --quiet --json

# Validate installation
if hs-jupyter-kernel doctor --quiet; then
  echo "Installation healthy"
else
  echo "Issues detected"
  exit 1
fi
```

### Multiple Jupyter Environments

The CLI automatically detects and works with:
- JupyterLab
- Jupyter Notebook
- Conda environments
- Virtual environments (venv, virtualenv)

## Migration from Legacy Scripts

If you previously used manual installation methods, see the [Migration Guide](docs/migration-from-install-script.md) for step-by-step instructions on moving to the new CLI.

## Building from Source

### Prerequisites

- **GHC 9.6.7+** via ghcup: `ghcup install ghc 9.6.7 && ghcup install cabal`
- **ZeroMQ library**: `libzmq3-dev` (Debian/Ubuntu) or equivalent for your OS
- **Python 3.8+** (optional) with `pyzmq` for the demo client

### Fast Development Build

```bash
# Build library only (5 seconds vs minutes)
cabal build lib:hs-jupyter-kernel -O0

# Full optimized build
cabal build

# Run tests
cabal test

# Install locally
cabal install
```

**⚡ Performance Note**: The hint library includes the full GHC API, resulting in long build times. For faster development, use `-O0` and the `ld.gold` linker configured in `cabal.project` (33x faster incremental builds: 77s → 2.3s).

### Running the Kernel Server

Start the kernel server with a Jupyter connection file:

```bash
cabal run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Info
```

### Testing

```bash
# Run targeted unit tests
cabal test unit -O0 --test-option="--match=/GHCSession/"

# Run all tests
cabal test

# Run integration tests with real GHC evaluation
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

## Status

### Current Implementation

- ✅ **Phase 1: Protocol Bridge** - Complete ZeroMQ integration with Jupyter messaging protocol
- ✅ **Phase 2: Runtime Core** - STM-based job queue, session state, resource management
- ✅ **Phase 3: GHC Evaluation** - Real Haskell evaluation with hint library, persistent sessions
- ✅ **Phase 4: Installation CLI** - Complete (install, doctor, list, uninstall, version commands)
- ⏳ **Phase 5: Advanced Features** - Planned (completions, debugging, package management)

### Test Coverage

- 97% test coverage (241/248 tests passing)
- 248 unit tests
- 60 integration tests
- Cross-platform compatibility tests
- Performance benchmarks

### Module Structure

- `HsJupyter.KernelProcess` – process lifecycle and socket management
- `HsJupyter.Bridge.*` – ZeroMQ bridge, heartbeat, protocol envelopes/codecs
- `HsJupyter.Router` – request routing and message dispatch
- `HsJupyter.Runtime.*` – runtime manager, GHC session, diagnostics, telemetry
- `HsJupyter.CLI.*` – installation, diagnostics, and management commands

## Troubleshooting

### Missing GHC/cabal

Install via ghcup:

```bash
ghcup install ghc 9.6.7
ghcup install cabal
```

Ensure they're on PATH.

### ZeroMQ library issues

Ensure libzmq is available on your system:

- Debian/Ubuntu: `sudo apt-get install libzmq3-dev`
- macOS: `brew install zeromq`
- Fedora: `sudo dnf install zeromq-devel`

### Use the doctor command

For any installation or runtime issues:

```bash
hs-jupyter-kernel doctor
```

This will diagnose and suggest fixes for common problems.

## Contributing

We welcome contributions! Please open an issue to discuss features, report bugs, or ask questions about the architecture.

## License

[License information to be added]
