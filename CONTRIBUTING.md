# Contributing to HsJupyter

Thank you for your interest in contributing to HsJupyter! This document provides guidelines and instructions for contributing to the project.

## Getting Started

### Prerequisites

- GHC 9.6.7+ (via ghcup)
- Cabal 3.0+
- ZeroMQ library (libzmq3-dev)
- Git

### Setting Up Development Environment

```bash
# Clone the repository
git clone https://github.com/jjunho/HsJupyter.git
cd HsJupyter

# Build the project (fast development build)
cabal build lib:hs-jupyter-kernel -O0

# Run tests
cabal test
```

See `docs/developer/README.md` for detailed development setup and build performance tips.

## Project Structure

```text
HsJupyter/
├── src/HsJupyter/         # Source code
│   ├── Bridge/            # Protocol layer
│   ├── Runtime/           # Execution engine
│   ├── Router/            # Message routing
│   ├── CLI/               # Command-line tools
│   └── Kernel/            # Core types
├── test/                  # Test suites
│   ├── unit/              # Unit tests
│   └── integration/       # Integration tests
├── docs/                  # Documentation
├── specs/                 # Design specifications
└── scripts/               # Helper scripts
```

## Development Workflow

### Branching Strategy

We use numbered feature branches:

```bash
# Create a feature branch
git checkout -b 001-feature-name

# Work in small commits
git commit -m "Add feature infrastructure"
git commit -m "Implement core functionality"
git commit -m "Add comprehensive tests"

# Push when ready for review
git push -u origin 001-feature-name
```

### Coding Standards

- **Haskell Style**: Four-space indentation, `HsJupyter.*` module namespace
- **Documentation**: Haddock comments for all public APIs
- **Testing**: Unit tests for new modules, integration tests for user-facing features
- **Principles**: Follow DRY, KISS, YAGNI, and SOLID principles (see `principles.md`)

### Testing Requirements

All contributions must include tests:

```bash
# Run all tests
cabal test

# Run specific test suite
cabal test unit
cabal test integration

# Run with detailed output
cabal test --test-show-details=streaming
```

### Documentation

- Update relevant documentation in `docs/` when adding features
- Add Haddock comments to new modules and functions
- Update `CHANGELOG.md` with your changes
- Ensure architecture documents reflect new components

## Pull Request Process

1. **Before submitting**:
   - Ensure all tests pass
   - Add appropriate documentation
   - Update CHANGELOG.md
   - Follow coding standards
   - Run `cabal check` and fix any warnings

2. **PR Description**:
   - Describe the changes and motivation
   - Reference related issues
   - Include testing notes
   - List any breaking changes

3. **Review Process**:
   - Maintainers will review your PR
   - Address feedback and requested changes
   - Once approved, your PR will be merged

## Code of Conduct

### Our Standards

- Be respectful and inclusive
- Focus on constructive feedback
- Help others learn and grow
- Maintain a professional environment

### Reporting Issues

If you encounter bugs or have feature requests:

1. Check existing issues first
2. Create a new issue with:
   - Clear description
   - Steps to reproduce (for bugs)
   - Expected vs actual behavior
   - Environment details (GHC version, OS, etc.)

## Architecture and Design

Before implementing major features:

1. Review `docs/architecture.md`
2. Check `docs/roadmap.md` for planned work
3. Discuss design in an issue or discussion
4. Follow the specification-driven development process (see `specs/`)

## Questions?

- Check `docs/developer/README.md` for development guides
- Review existing issues and discussions
- Ask questions in GitHub Discussions

## License

By contributing to HsJupyter, you agree that your contributions will be licensed under the MIT License.

## Thank You

Your contributions help make HsJupyter better for everyone. We appreciate your time and effort!
