# Changelog

All notable changes to HsJupyter will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- Documentation updates (README, roadmap, architecture guides)
- Enhanced Haddock documentation for core modules
- Improved code quality (DRY, KISS, YAGNI compliance)

### Fixed

- Code violations of DRY, KISS, and YAGNI principles
- Test reliability and correctness improvements
- Import handling in diagnostics and resource guard modules

### Added

- CHANGELOG.md and CONTRIBUTING.md for project maintenance
- Enhanced .gitignore for Jupyter notebooks and Python artifacts
- Comprehensive cabal metadata with proper bounds and documentation

## [0.1.0] - 2025-10-26

First production-ready release with complete kernel functionality.

### Phase 4: Installation & CLI Infrastructure (October 25-26, 2025)

#### Phase 4 Features

- CLI command infrastructure with optparse-applicative
- Installation command with Jupyter kernelspec registration
- Doctor command for diagnostics and troubleshooting
- Configuration management system
- System detection utilities (Jupyter, Python, GHC)
- Kernelspec directory discovery and validation
- kernel.json generation with resource limits
- Comprehensive CLI unit and integration tests
- TMVar-based cancellation support for CLI operations
- Structured logging via telemetry patterns

#### Phase 4 Documentation

- CLI architecture documentation
- Usage guides and troubleshooting
- Installation & CLI specification

### Phase 3: GHC Evaluation (October 25, 2025)

#### Phase 3 Features

- Real Haskell evaluation using hint library (GHC API)
- Persistent GHC sessions with InterpreterT monad
- Variable and function binding persistence across cells
- Module import system with security policies
- Safe module whitelist (Prelude, Data.*, Control.*, etc.)
- Import policy enforcement (allow/deny/default)
- Comprehensive error handling with 5 syntax error types:
  - Parse errors with line/column information
  - Type errors with context
  - Runtime exceptions
  - Timeout errors
  - Import policy violations
- Smart error diagnostics with actionable suggestions
- Performance monitoring with differentiated timeouts:
  - 3s for simple expressions
  - 5s for declarations
  - 30s for module imports
- Memory monitoring via RTSStats integration
- TMVar-based cancellation infrastructure
- ResourceGuard integration with violation handling
- Performance telemetry collection

#### Phase 3 Testing

- 135+ unit test examples for GHC functionality
- 33+ integration tests for notebook workflows
- Comprehensive test coverage for all evaluation paths

### Phase 2: Runtime Core (October 24, 2025)

#### Phase 2 Features

- STM-based job queue with bounded capacity
- Concurrent runtime manager with worker threads
- Session state management across cell executions
- Execution count tracking
- Binding persistence (variables, functions)
- Module artifact caching
- Import list management
- Resource management infrastructure:
  - Memory limits (RSS and virtual memory)
  - CPU timeouts (wall clock and user time)
  - Output size limits with truncation
- ResourceGuard with background monitoring
- Job registry with TMVar cancellation tokens
- Execution diagnostics and error reporting
- Runtime telemetry and metrics collection
- Comprehensive unit tests for all runtime components
- Integration tests for execute cycle

### Phase 1: Protocol Bridge (October 24, 2025)

#### Phase 1 Features

- Complete Jupyter kernel protocol implementation
- ZeroMQ socket management for all 5 channels:
  - Shell (ROUTER) - execution requests
  - IOPub (PUB) - output broadcasting
  - Control (ROUTER) - interrupt/shutdown
  - Stdin (ROUTER) - input requests
  - Heartbeat (REP) - health checks
- HMAC-SHA256 signature validation
- Protocol envelope parsing and serialization
- Message codec for Jupyter wire format
- Typed message structures (execute_request, execute_reply, etc.)
- kernel_info_request/reply handling
- execute_request/reply handling
- interrupt_request/reply handling
- Heartbeat thread with automatic echo
- Structured logging with correlation IDs
- Connection file parsing
- Echo runtime for protocol validation
- Python demo client for testing

#### Phase 1 Infrastructure

- Initial project structure and cabal configuration
- Test infrastructure (hspec, QuickCheck)
- CI-ready build system
- Documentation framework (specs/, docs/)
- Development scripts and tooling

### Constitutional Compliance

#### Constitutional Principles Implementation

- Constitution v1.2.0 with four core principles:
  1. Simplicity and Clarity (DRY, KISS, YAGNI)
  2. Strong Design Foundation (SOLID, SoC, Composition)
  3. Resilience and Maintainability (Defensive Programming, Law of Demeter)
  4. Balance and Pragmatism (Rule of Three, Cohesion/Coupling)
- Comprehensive compliance implementation across codebase
- Shared error handling patterns (DRY)
- Modular architecture with clear boundaries (SOLID)
- 147+ test examples with comprehensive validation
- Documentation-first development process

### Project Documentation

- Architecture overview and design decisions
- Roadmap with phase tracking
- Developer guide with build performance tips
- Jupyter kernel implementation guide
- Protocol specifications and contracts
- Testing requirements and guidelines
- API documentation with Haddock comments

### Project Dependencies

- GHC 9.6.7+
- hint >= 0.9.0 (GHC API integration)
- zeromq4-haskell >= 0.8 (Jupyter protocol)
- aeson >= 2.0 (JSON handling)
- katip (structured logging)
- stm >= 2.5 (concurrency primitives)
- cryptonite >= 0.30 (HMAC signatures)
- optparse-applicative >= 0.16 (CLI parsing)

## [0.0.1] - 2025-10-24

### Initial Release

- Initial project scaffold from Specify template
- Basic project structure and build system
- License (MIT) and initial documentation

[Unreleased]: https://github.com/jjunho/HsJupyter/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/jjunho/HsJupyter/releases/tag/v0.1.0
[0.0.1]: https://github.com/jjunho/HsJupyter/releases/tag/v0.0.1
