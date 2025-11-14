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
- Custom installation paths (`--jupyter-dir`, `--kernelspec-dir`)
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

## [0.4.0] - 2025-11-10

### Fixed

- **Critical: Kernel communication protocol bugs** (2025-11-14, branch `005-fix-jupyter-kernel-bug`)
  - Fixed three bugs preventing Jupyter protocol message exchange:
    1. **LogEnv configuration**: `mkBridgeContext` now properly respects log level from `--log-level` flag and `HSJUPYTER_LOG_LEVEL` environment variable by using `toKatipSeverity` converter and `permitItem` filtering
    2. **Protocol delimiter parsing**: `parseEnvelopeFrames` now accepts empty frame (`b''`) as delimiter per Jupyter protocol specification, in addition to legacy `<IDS|MSG>` string
    3. **Reply envelope construction**: `replyEnvelope` uses `signaturePayloadFrom` to construct valid signature payloads instead of `undefined` placeholder
  - Result: Kernel successfully receives `kernel_info_request` and sends `kernel_info_reply`, establishing working Jupyter protocol communication

### Changed

- **Build performance optimization** (2025-11-14)
  - Configured `ld.gold` linker in `cabal.project` for dramatically faster builds
  - Incremental builds: 77s → 2.3s (33x speedup)
  - Full clean builds: ~18s with `-O0`

### Added

- Retrospective documentation for kernel integration bug fix (076a1bf, d892702)

### Technical Details

- Modified `src/HsJupyter/Bridge/JupyterBridge.hs`: Added `toKatipSeverity`, `mkHandleScribe`, and `registerScribe`
- Modified `src/HsJupyter/Bridge/Protocol/Codec.hs`: Updated delimiter detection, added `decodeParentHeader`
- Modified `src/HsJupyter/Router.hs`: Fixed `replyEnvelope` signature payload
- Modified `cabal.project`: Added `ld.gold` linker configuration

## [0.3.0] - 2025-11-04

### Fixed

- Test compilation errors in protocol bridge and runtime modules (5b25d3f, 9d3512f)
- Resource limit test reliability and correctness (9ea0516, e2a1e3e)

### Added

- Specification, plan, and tasks for Jupyter kernel bug fix (076a1bf)
- Initial Jupyter kernel bug fix implementation (8c6963b)

## [0.2.0] - 2025-10-26

### Added

- **Phase 4: Installation & CLI Infrastructure** - Initial implementation
  - CLI command infrastructure with `optparse-applicative` (942a212, 2f4d057)
  - Jupyter environment detection (a7d6b9f)
  - Kernelspec directory discovery and validation (68c4d8f)
  - `kernel.json` generation with constitutional compliance (68c4d8f)
  - System detection and configuration management (e5a4814)
  - CLI data models and error types (ad19681)
  - Constitutional audit integration (23462f8)
  - Doctor and install command implementations

### Changed

- Auto-detect `hs-jupyter-kernel` binary in install-kernelspec script (a954cd4, b5c6c03)
- Use explicit `--name hsjupyter` for consistent kernelspec naming (b5c6c03)

### Fixed

- CLI/GHC diagnostics and resource guard improvements (34c1227)
- Import handling cleanups (34c1227)

## [0.1.0] - 2025-10-25

### Added

- **Phase 3: GHC Evaluation** - Complete implementation (145cf4a)
  - Basic expression evaluation via hint library (ca8854b)
  - Persistent GHC session across evaluations (ca8854b)
  - Module import system with security policy (75af371)
  - Error handling and diagnostics (d2699f1)
  - Resource management and performance optimization (7d9d87d)
  - Build optimizations for development workflow (ca8854b)
- Constitution v1.2.0 compliance improvements (966f037)
  - Resilience and pragmatic balance principles (c3bcfdb, a865bab)
- Kernel testing infrastructure scripts (f22df3c)

### Changed

- Updated roadmap to reflect Phase 3 completion (950de20)
- Made README immediately usable with quickstart, CLI, demo, troubleshooting (0127606)

## [0.0.2] - 2025-10-24

### Added

- **Phase 2: Runtime Core** - Complete implementation (4b0f849)
  - Runtime manager and session state (be6ff23)
  - Evaluation engine (e62cfdc)
  - Integration tests and test infrastructure (43ff2d2)
  - Diagnostics and resource guards (8a435c2)
  - Comprehensive documentation (e45c720)
  - All 25/25 tasks delivered (69601bd)
- Strengthened resource guard metrics and RTS stats (6872d0f, 465f23b)

### Changed

- Updated protocol bridge telemetry and templates (d0435d0)

## [0.0.1] - 2025-10-24

### Added

- **Phase 1: Protocol Bridge** - Initial implementation
  - ZeroMQ-enabled kernel prototype (31939e2)
  - Protocol demo and specifications (2a7b2ec)
  - Kernel loops with bind guards and heartbeat checks (8118328)
  - Runtime dependencies and tooling (f12ff46)
  - Runtime foundational scaffolding (3edca05)
- Initial project structure from Specify template (060d743)

### Supported

- Jupyter protocol 5.3
- GHC 9.6.7+ (via ghcup)
- Platforms: Linux, macOS
- Python 3.8+ for Jupyter integration
- Test coverage: 248 unit tests, 60 integration tests

[Unreleased]: https://github.com/jjunho/HsJupyter/compare/v0.4.0...HEAD
[0.4.0]: https://github.com/jjunho/HsJupyter/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/jjunho/HsJupyter/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/jjunho/HsJupyter/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/jjunho/HsJupyter/compare/v0.0.2...v0.1.0
[0.0.2]: https://github.com/jjunho/HsJupyter/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/jjunho/HsJupyter/releases/tag/v0.0.1
