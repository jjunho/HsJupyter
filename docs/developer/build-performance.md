# Build Performance Guide

## Overview

HsJupyter builds can be slow due to the **hint library** (GHC API integration). This guide provides optimization strategies for development workflows.

## Performance Issues

### Root Causes

- **hint library**: Includes full GHC API (~100MB+ dependencies)
- **Static linking**: Combines all 292+ dependencies into executable
- **GHC 9.12.2**: Newer GHC versions have compilation overhead
- **Optimization levels**: `-O1`/`-O2` perform extensive analysis

### Typical Build Times

**With optimizations (default ld linker):**

- **Full build with optimization**: 3-5 minutes
- **Incremental builds**: 30 seconds - 2 minutes

**With ld.gold linker (recommended):**

- **Full build (`-O0`)**: ~18 seconds
- **Incremental builds**: 2-3 seconds (33x faster!)
- **Library-only build**: 5-10 seconds

**Note**: Using `ld.gold` provides dramatic linking speedup. See configuration below.

## Fast Development Workflow

### 1. Quick Compilation Check

```bash
# Fastest: Library only, no optimization
cabal build lib:hs-jupyter-kernel -O0
```

### 2. Targeted Testing

```bash
# Run specific test modules
cabal test unit -O0 --test-option="--match=/GHCSession/"
cabal test unit -O0 --test-option="--match=/Diagnostics/"

# Skip integration tests during active development
cabal test unit -O0
```

### 3. Incremental Development

```bash
# 1. Check compilation
cabal build lib:hs-jupyter-kernel -O0

# 2. Test specific functionality
cabal test unit -O0 --test-option="--match=/YourModule/"

# 3. Full validation (when needed)
cabal test integration -O0

# 4. Production build (final step)
cabal build  # With optimizations
```

## Build Configuration

### Linker Optimization (Highly Recommended)

Using `ld.gold` provides **33x faster** incremental builds compared to the default linker.

Add to `cabal.project`:

```cabal
-- Use faster linker (gold is significantly faster than default ld)
program-options
  ghc-options: -optl-fuse-ld=gold
```

**Performance comparison:**

- Default `ld` linker: 77 seconds for incremental rebuild
- `ld.gold` linker: 2.3 seconds for incremental rebuild

**Installation** (if not already present):

```bash
# Ubuntu/Debian
sudo apt-get install binutils-gold

# Verify availability
which ld.gold
```

### Project-Level Optimizations

Add to `cabal.project`:

```cabal
-- Performance optimizations
jobs: 4
documentation: False
haddock-all: False
optimization: 1
split-sections: True
```

### User-Level Configuration

Add to `~/.cabal/config`:

```cabal
jobs: 4
documentation: False
```

### Environment Variables

```bash
# Use multiple cores for compilation
export CABAL_BUILD_JOBS=4

# Disable documentation generation
export CABAL_BUILD_DOCS=False
```

## Development Tools

### ghcid for Instant Feedback

```bash
# Install once
cabal install ghcid

# Use for instant recompilation feedback
ghcid --command="cabal repl lib:hs-jupyter-kernel"
```

### Build Time Monitoring

```bash
# Time your builds
time cabal build lib:hs-jupyter-kernel -O0

# Monitor resource usage
cabal build -v2 | grep "Running:"
```

## Command Quick Reference

### Fast Commands (Development)

```bash
# Compilation check (5-15 seconds)
cabal build lib:hs-jupyter-kernel -O0

# Quick test run (30-60 seconds)
cabal test unit -O0

# Dependencies only (when cabal.project changes)
cabal build --dependencies-only
```

### Standard Commands (CI/Production)

```bash
# Full build with optimizations (3-5 minutes)
cabal build

# All tests (5-10 minutes)
cabal test

# Clean build (when needed)
cabal clean && cabal build
```

### Debugging Slow Builds

```bash
# Verbose output to identify bottlenecks
cabal build --verbose=2

# Check dependency tree
cabal list --installed | wc -l

# Profile build times
cabal build --enable-profiling +RTS -p
```

## Best Practices

### During Feature Development

1. **Start with library builds**: `cabal build lib:hs-jupyter-kernel -O0`
2. **Use targeted tests**: Only run tests for modules you're changing
3. **Avoid full builds**: Only run full builds when preparing for review
4. **Use ghcid**: For instant feedback on syntax/type errors

### Before Commits

1. **Clean library build**: Ensure no compilation errors
2. **Full test suite**: Run all tests with optimizations
3. **Integration tests**: Verify end-to-end functionality

### CI/Production

1. **Use optimizations**: Default build flags for performance
2. **Full test coverage**: All unit and integration tests
3. **Clean builds**: Start from clean state

## Troubleshooting

### "Build taking forever"

- Check if you're running with optimizations (`-O1`, `-O2`)
- Use `-O0` for development builds
- Ensure parallel builds are enabled (`jobs: 4`)

### "Out of memory during linking"

- Reduce optimization level
- Use `split-sections: True` in cabal.project
- Consider using dynamic linking for development

### "Tests timing out"

- Run tests without optimizations (`-O0`)
- Use targeted test execution
- Check if resource limits are too strict

## Integration with Specify Toolkit

When working with specify toolkit workflows:

```bash
# After /speckit.implement
cabal build lib:hs-jupyter-kernel -O0  # Quick check

# Targeted testing for new functionality  
cabal test unit -O0 --test-option="--match=/NewModule/"

# Full validation before marking complete
cabal test integration -O0
```

This ensures fast iteration while maintaining quality validation.
