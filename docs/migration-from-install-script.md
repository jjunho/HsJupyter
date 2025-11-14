# Migration Guide: From Legacy Install Script to CLI

This guide helps users migrate from manual installation methods (shell scripts, manual kernel.json editing) to the new unified CLI interface.

## Overview

The new CLI provides a comprehensive, cross-platform installation and management system that replaces manual installation procedures with simple commands.

---

## Quick Migration

### Before (Legacy Manual Installation)

```bash
# Manual steps required
jupyter kernelspec install kernelspec/hsjupyter --user
# Manual kernel.json editing
# Manual path configuration
# Manual troubleshooting
```

### After (New CLI)

```bash
# Single command installation
hs-jupyter-kernel install --user

# Automatic configuration
# Automatic validation
# Built-in diagnostics
```

---

## Detailed Migration Steps

### Step 1: Verify Installation

Before migrating, check your current installation:

```bash
jupyter kernelspec list
```

Look for `hsjupyter` in the output. Note the installation path.

### Step 2: Install Using New CLI

#### For User Installation (Most Common)

**Old Method**:
```bash
# Manual kernelspec copy
cp -r kernelspec/hsjupyter ~/.local/share/jupyter/kernels/
# Manual kernel.json editing
```

**New Method**:
```bash
hs-jupyter-kernel install --user
```

#### For System-Wide Installation

**Old Method**:
```bash
# Required sudo and manual path configuration
sudo jupyter kernelspec install kernelspec/hsjupyter --sys-prefix
```

**New Method**:
```bash
hs-jupyter-kernel install --system
```

### Step 3: Verify New Installation

```bash
# Check installation status
hs-jupyter-kernel list

# Run diagnostics
hs-jupyter-kernel doctor
```

### Step 4: Clean Up Old Installation (Optional)

If you want to remove the old manual installation:

```bash
# Remove old installation
hs-jupyter-kernel uninstall

# Or manually:
# jupyter kernelspec uninstall hsjupyter
```

---

## Feature Comparison

| Feature | Legacy Method | New CLI | Benefits |
|---------|---------------|---------|----------|
| **Installation** | Manual copy + edit | `install` command | Automatic, validated |
| **Path Detection** | Manual configuration | Automatic detection | No manual setup |
| **GHC Detection** | Manual specification | Automatic discovery | Works out of the box |
| **Troubleshooting** | Trial and error | `doctor` command | Guided diagnostics |
| **Updates** | Manual reinstall | `install --force` | Simple updates |
| **Multiple Versions** | Complex management | `list` command | Easy tracking |
| **Uninstall** | Manual deletion | `uninstall` command | Safe removal |
| **Validation** | None | Automatic | Catches issues early |
| **Cross-Platform** | OS-specific scripts | Single CLI | Works everywhere |

---

## Common Migration Scenarios

### Scenario 1: Simple User Installation

**Before**:
```bash
# Manual steps
mkdir -p ~/.local/share/jupyter/kernels/hsjupyter
cp kernel.json ~/.local/share/jupyter/kernels/hsjupyter/
# Edit kernel.json manually
vim ~/.local/share/jupyter/kernels/hsjupyter/kernel.json
```

**After**:
```bash
hs-jupyter-kernel install --user
```

### Scenario 2: Custom GHC Path

**Before**:
```bash
# Edit kernel.json
{
  "argv": ["/custom/path/to/ghc", ...],
  ...
}
```

**After**:
```bash
hs-jupyter-kernel install --ghc-path /custom/path/to/ghc
```

### Scenario 3: Custom Display Name

**Before**:
```bash
# Edit kernel.json
{
  "display_name": "My Custom Haskell",
  ...
}
```

**After**:
```bash
hs-jupyter-kernel install --display-name "My Custom Haskell"
```

### Scenario 4: Resource Limits

**Before**:
```bash
# Manual kernel.json editing
{
  "metadata": {
    "resource_limits": {
      "memory": "2048",
      "timeout": "300"
    }
  }
}
```

**After**:
```bash
hs-jupyter-kernel install \
  --memory-limit 2048 \
  --exec-timeout 300
```

### Scenario 5: Troubleshooting Installation Issues

**Before**:
```bash
# Manual debugging
cat ~/.local/share/jupyter/kernels/hsjupyter/kernel.json
which ghc
jupyter kernelspec list
# Trial and error
```

**After**:
```bash
# Automated diagnostics
hs-jupyter-kernel doctor

# Get detailed report
hs-jupyter-kernel doctor --report diagnostic-report.json
```

---

## Advanced Configuration

### Custom Kernelspec Directory

**Before**:
```bash
# Set environment variable
export JUPYTER_DATA_DIR=/custom/path
jupyter kernelspec install ...
```

**After**:
```bash
hs-jupyter-kernel install --kernelspec-dir /custom/path/kernels
```

### Multiple Jupyter Environments (Conda)

**Before**:
```bash
# Activate conda environment
conda activate myenv
# Manual installation per environment
jupyter kernelspec install ...
```

**After**:
```bash
# Activate conda environment
conda activate myenv
# CLI automatically detects conda environment
hs-jupyter-kernel install
```

### Configuration Files

For complex setups, create a configuration file:

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

Then install using:
```bash
hs-jupyter-kernel install --config my-config.json
```

---

## Automation & CI/CD

### Legacy Script in CI

**Before**:
```yaml
# .github/workflows/test.yml
- name: Install kernel
  run: |
    ./scripts/install-kernelspec.sh
    # Hope it works
```

**After**:
```yaml
# .github/workflows/test.yml
- name: Install kernel
  run: |
    hs-jupyter-kernel install --quiet --json
    # Parse JSON output for validation
```

### Programmatic Validation

**New CLI Benefits**:
```bash
# JSON output for parsing
hs-jupyter-kernel list --json | jq '.functional'

# Exit codes for scripting
if hs-jupyter-kernel doctor --quiet; then
  echo "Installation healthy"
else
  echo "Issues detected"
  exit 1
fi
```

---

## Troubleshooting Migration

### Issue: "kernel.json not found"

**Solution**:
```bash
# Run diagnostics first
hs-jupyter-kernel doctor

# Follow recommendations
hs-jupyter-kernel install --validation full
```

### Issue: "GHC not found"

**Solution**:
```bash
# Check system
hs-jupyter-kernel doctor --check ghc

# Specify custom path
hs-jupyter-kernel install --ghc-path $(which ghc)
```

### Issue: "Permission denied"

**Solution**:
```bash
# Use user installation instead of system
hs-jupyter-kernel install --user
```

### Issue: Multiple conflicting installations

**Solution**:
```bash
# List all installations
hs-jupyter-kernel list --all

# Remove all
hs-jupyter-kernel uninstall --all --confirm

# Fresh install
hs-jupyter-kernel install
```

---

## Best Practices

### 1. Always Use CLI for Installation
✅ **Do**: `hs-jupyter-kernel install`
❌ **Don't**: Manual kernel.json copying

### 2. Run Diagnostics Before Troubleshooting
✅ **Do**: `hs-jupyter-kernel doctor`
❌ **Don't**: Manual debugging without diagnosis

### 3. Use JSON Output for Automation
✅ **Do**: `hs-jupyter-kernel list --json`
❌ **Don't**: Parse human-readable output

### 4. Version Control Configuration
✅ **Do**: Use `--config` files for custom setups
❌ **Don't**: Manual kernel.json editing

### 5. Validate After Installation
✅ **Do**: `hs-jupyter-kernel install --validation full`
❌ **Don't**: Assume installation worked

---

## Getting Help

### Documentation
- Full command reference: `hs-jupyter-kernel --help`
- Command-specific help: `hs-jupyter-kernel install --help`

### Diagnostics
```bash
# Comprehensive diagnostics
hs-jupyter-kernel doctor

# Component-specific checks
hs-jupyter-kernel doctor --check jupyter
hs-jupyter-kernel doctor --check kernel
hs-jupyter-kernel doctor --check ghc
```

### Reporting Issues

Include output from:
```bash
hs-jupyter-kernel version
hs-jupyter-kernel doctor --json > diagnostic-report.json
hs-jupyter-kernel list --json > installations.json
```

---

## Summary

The new CLI provides:
- ✅ Automatic environment detection
- ✅ Comprehensive diagnostics
- ✅ Simple command interface
- ✅ Cross-platform support
- ✅ JSON output for automation
- ✅ Safe installation and removal
- ✅ Built-in validation

**Migration is simple**: Replace manual procedures with `hs-jupyter-kernel install`!
