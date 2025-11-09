# CLI Architecture Documentation

**Feature**: Installation & CLI Infrastructure  
**Created**: 2025-01-28  
**Status**: In Development

## Overview

The HsJupyter CLI extends the existing kernel executable with installation and management commands while preserving the original kernel server functionality.

## Architecture Decisions

### Command Dispatch Strategy

The CLI uses argument-based mode detection in `app/KernelMain.hs`:

- **Kernel Server Mode**: Default behavior when no CLI commands detected
- **CLI Command Mode**: Activated by recognized subcommands (`install`, `doctor`, `uninstall`, `list`, `version`)

### Module Organization

Following constitutional patterns with clear separation of concerns:

```text
src/HsJupyter/CLI/
├── Commands.hs         # Command parsing and dispatch
├── Types.hs           # Core data models and error types  
├── Install.hs         # Installation logic
├── Doctor.hs          # Diagnostic functionality
├── Config.hs          # Configuration management
└── System.hs          # System integration utilities
```

### Constitutional Integration

All CLI functionality integrates with existing constitutional framework:

- **Error Handling**: Extends `RuntimeDiagnostic` system
- **Observability**: Uses existing `katip` structured logging
- **Resource Management**: Integrates with `ResourceGuard` patterns
- **Cancellation**: Supports `TMVar`-based cancellation

## Design Principles

### 1. Backward Compatibility

The kernel server functionality remains unchanged. Users can continue using:

```bash
hs-jupyter-kernel --connection connection.json
```

### 2. Constitutional Compliance

All CLI code follows established patterns:

- Modular architecture with SOLID principles
- DRY error handling through shared utilities
- Comprehensive logging and observability
- Defensive programming with input validation

### 3. Cross-Platform Support

CLI operations work across Linux, macOS, and Windows:

- Use `System.FilePath` for path operations
- Handle platform-specific Jupyter installations
- Support various Python environments (conda, pip, system)

## Implementation Status

- [x] Basic CLI infrastructure (argument parsing, mode detection)
- [ ] Core data models and error types
- [ ] Installation command implementation
- [ ] Diagnostic command implementation  
- [ ] Configuration and system utilities
- [ ] Integration testing and validation

## Future Enhancements

Planned for subsequent user stories:

- Custom installation configuration
- JSON output for programmatic access
- Advanced diagnostic capabilities
- Performance optimization and monitoring
