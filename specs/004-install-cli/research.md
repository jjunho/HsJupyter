# Research: Installation & CLI Infrastructure

**Feature**: 004-install-cli  
**Date**: 2025-01-28  
**Status**: Complete

## Research Findings

### CLI Framework Choice

**Decision**: Use `optparse-applicative` for command-line interface parsing

**Rationale**:

- Well-established Haskell library with excellent type safety
- Automatic help generation and subcommand support
- Integrates cleanly with existing constitutional error handling patterns
- Minimal dependencies beyond what HsJupyter already requires

**Alternatives considered**:

- Custom argument parsing: Rejected due to complexity and maintenance burden
- `cmdargs`: Rejected due to less type-safe interface and template Haskell dependencies

### Jupyter Integration Strategy

**Decision**: Direct kernelspec directory manipulation with validation

**Rationale**:

- Jupyter's kernelspec system is well-documented and stable across versions
- Direct filesystem approach avoids Python dependency for basic operations
- Enables custom path support for non-standard Jupyter installations
- Aligns with constitutional simplicity principles (KISS, YAGNI)

**Alternatives considered**:

- `jupyter kernelspec install` subprocess calls: Rejected due to Python dependency requirement
- Python wrapper scripts: Rejected due to additional complexity and deployment burden

### System Integration Approach

**Decision**: Use Haskell `process`, `directory`, and `filepath` libraries for system operations

**Rationale**:

- Cross-platform compatibility for Linux, macOS, Windows
- Direct integration with existing Haskell codebase
- Constitutional compliance with defensive programming (proper error handling)
- Leverages existing ResourceGuard patterns for timeouts and resource limits

**Alternatives considered**:

- Shell script wrappers: Rejected due to platform compatibility issues
- External system utilities: Rejected due to dependency management complexity

### Error Handling Integration

**Decision**: Extend existing `RuntimeDiagnostic` system for CLI-specific errors

**Rationale**:

- Maintains consistency with established constitutional error handling patterns
- Leverages existing structured logging through katip
- Enables proper error propagation and user-friendly messages
- Supports diagnostic command requirements with detailed error reporting

**Alternatives considered**:

- Separate CLI error system: Rejected due to duplication and constitutional DRY violations
- Basic string-based errors: Rejected due to lack of structure and observability

### Configuration Management

**Decision**: JSON-based kernel.json generation with validation

**Rationale**:

- Standard Jupyter kernel specification format
- Leverages existing `aeson` JSON handling infrastructure
- Enables custom configuration while maintaining compatibility
- Supports constitutional validation and defensive programming principles

**Alternatives considered**:

- YAML configuration: Rejected due to additional dependencies and complexity
- INI-style configuration: Rejected due to limited structure and nesting capabilities

### Performance Optimization Strategy

**Decision**: Lazy evaluation with strict error boundaries

**Rationale**:

- Installation operations benefit from lazy I/O for large directory scans
- Constitutional performance targets (<2min install, <5s diagnostics) achievable
- Existing constitutional patterns for resource limits and timeouts apply
- Minimal memory footprint aligns with <100MB constraint

**Alternatives considered**:

- Strict evaluation throughout: Rejected due to memory usage concerns for large Jupyter environments
- Streaming I/O libraries: Rejected as premature optimization for typical installation scenarios

### Testing Strategy

**Decision**: Mirror existing test structure with CLI-specific integration tests

**Rationale**:

- Follows constitutional test-first implementation principles
- Leverages existing hspec infrastructure and patterns
- Integration tests can validate end-to-end Jupyter interaction
- Unit tests ensure individual command functionality

**Alternatives considered**:

- Golden tests for CLI output: Considered but deferred to implementation phase based on output complexity
- Mock-based testing: Rejected in favor of real filesystem integration for reliability

## Implementation Dependencies

### Internal Dependencies

- Existing `HsJupyter.Runtime.ErrorHandling` module for error patterns
- Existing `HsJupyter.Runtime.Telemetry` for observability integration
- Existing `katip` structured logging infrastructure
- Constitutional test patterns from established modules

### External Dependencies

- `optparse-applicative` (CLI parsing)
- `process` (system command execution)
- `directory` (filesystem operations)
- `filepath` (cross-platform path handling)
- Standard base libraries (no additional package dependencies required)

### System Dependencies

- Jupyter installation (Lab or Notebook) for integration testing
- GHC 9.12.2+ for kernel verification during installation
- Standard shell environment (bash/zsh/cmd) for process execution

## Risk Assessment

### Low Risk

- CLI command parsing and validation
- JSON kernel.json file generation
- Basic filesystem operations for kernelspec management

### Medium Risk

- Cross-platform path handling for various Jupyter installation types
- Jupyter environment detection across different Python installations (conda, pip, system)
- Permission handling for system vs user installation modes

### High Risk

- None identified with current approach and constitutional compliance

## Performance Validation Approach

### Benchmarking Strategy

- Installation time measurement on clean systems with various Jupyter setups
- Diagnostic command response time validation
- Memory usage profiling during installation operations
- Timeout behavior validation for network-dependent operations

### Success Criteria Validation

- SC-001: <2 minutes installation → measure on standard test systems
- SC-002: 95% success rate → automated testing across platform variations
- SC-003: 90% issue resolution via doctor → diagnostic accuracy measurement
- SC-004: <30 seconds with dependencies → dependency detection timing
- SC-005: <5 seconds diagnostics → response time benchmarking
- SC-006: <10 seconds kernel availability → Jupyter integration timing
- SC-007: 100% uninstall success → cleanup verification testing
- SC-008: <100MB temporary usage → disk space monitoring

## Next Steps

Research complete. All technical decisions documented with clear rationale. Ready to proceed to Phase 1 (Design & Contracts) with comprehensive foundation for implementation planning.
