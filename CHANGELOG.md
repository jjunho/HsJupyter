# Changelog

All notable changes to HsJupyter will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added - Installation & CLI Infrastructure (Feature 004)

#### 🎯 Core Commands
- **`hs-jupyter-kernel install`** - Simple kernel installation with automatic Jupyter environment detection
- **`hs-jupyter-kernel doctor`** - Comprehensive system diagnostics and troubleshooting
- **`hs-jupyter-kernel list`** - Enumerate all installed HsJupyter kernels
- **`hs-jupyter-kernel uninstall`** - Safe kernel removal with confirmation prompts
- **`hs-jupyter-kernel version`** - Display version and compatibility information

#### 🔧 Installation Features
- Automatic Jupyter environment detection (JupyterLab, Notebook, conda environments)
- User and system-wide installation scopes (`--user`, `--system`)
- Force reinstallation for updates (`--force`)
- Custom installation paths (`--jupyter-dir`, --kernelspec-dir`)
- Custom GHC path specification (`--ghc-path`)
- Installation validation levels (none, basic, full)
- Structured logging with katip integration
- Resource guards for timeout and disk space management

#### 🔍 Diagnostics Features
- Multi-component health checks (Jupyter, Kernel, GHC, System)
- Issue severity classification (Critical, Major, Minor, Warning)
- Actionable recommendations with specific commands
- System information collection (platform, architecture, PATH)
- Component-specific diagnostics (`--check jupyter|kernel|ghc|system|all`)
- Automatic fix attempts (`--fix` flag)
- Detailed diagnostic reports (`--report FILE`)

#### ⚙️ Custom Configuration (Advanced)
- Custom kernel display names (`--display-name`)
- Resource limit configuration (memory, timeout, output size)
- Environment variables injection (`--env KEY=VALUE`)
- Additional kernel arguments (`--kernel-arg ARG`)
- Configuration file support (`--config FILE`)
- Custom language identifiers

#### 🤖 Automation & Integration
- JSON output mode for all commands (`--json`)
- Quiet mode for non-interactive execution (`--quiet`)
- Verbose logging mode (`--verbose`)
- Programmatic exit codes for CI/CD integration
- Multiple installation enumeration
- Safe uninstall with confirmation prompts

#### 📊 Performance
- Installation: <30 seconds (with dependencies available)
- Diagnostics: <1 second
- List command: <1 second
- Version command: <0.1 second
- Memory usage: <100MB during operations

#### ✅ Constitutional Compliance
- Modular architecture following SOLID principles
- Comprehensive error handling with structured diagnostics
- Observability through katip structured logging
- Resource management with guards and timeouts
- Defensive programming with input validation
- Cross-platform support (Linux, macOS, Windows)

#### 🧪 Testing
- 248 unit tests (97% passing)
- Integration tests for end-to-end workflows
- Cross-platform compatibility tests
- JSON schema validation
- Performance benchmarks

### Changed
- Main executable now supports CLI subcommands in addition to kernel server mode
- Kernel server mode remains default when no command specified (backward compatible)

### Migration Guide
See [docs/migration-from-install-script.md](docs/migration-from-install-script.md) for migration from legacy `scripts/install-kernelspec.sh`.

---

## [0.1.0.0] - 2025-01-14

### Added
- Initial release of HsJupyter kernel
- Jupyter protocol implementation
- GHC integration for code evaluation
- Resource management and timeout controls
- Structured logging with katip
- Constitutional error handling framework

[Unreleased]: https://github.com/yourusername/HsJupyter/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/yourusername/HsJupyter/releases/tag/v0.1.0.0
