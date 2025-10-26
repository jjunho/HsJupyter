# HsJupyter CLI Usage Guide

**Version**: 0.1.0.0
**Last Updated**: 2025-01-28

## Overview

The HsJupyter CLI provides tools for installing, managing, and diagnosing
Haskell Jupyter kernel installations. This guide covers all available commands
and their usage patterns.

## Quick Start

### Installation

```bash
# Install HsJupyter kernel for current user
hs-jupyter-kernel install

# Install system-wide (requires administrator privileges)
hs-jupyter-kernel install --system

# Force reinstallation
hs-jupyter-kernel install --force
```

### Diagnostics

```bash
# Check installation health
hs-jupyter-kernel doctor

# Get detailed diagnostic information
hs-jupyter-kernel doctor --verbose

# Save diagnostic report to file
hs-jupyter-kernel doctor --report diagnostic-report.json
```

### Management

```bash
# List all HsJupyter installations
hs-jupyter-kernel list

# Show version information
hs-jupyter-kernel version

# Uninstall HsJupyter kernel
hs-jupyter-kernel uninstall
```

## Command Reference

### `install` - Install HsJupyter Kernel

Install the Haskell Jupyter kernel to your system.

```bash
hs-jupyter-kernel install [OPTIONS]
```

#### Install Options

- `--user`: Install for current user only (default)
- `--system`: Install system-wide (requires administrator privileges)
- `--force`: Force overwrite existing installation
- `--quiet`: Suppress non-essential output
- `--json`: Output results in JSON format
- `--display-name NAME`: Custom kernel display name
- `--ghc-path PATH`: Path to GHC executable
- `--jupyter-dir DIR`: Custom Jupyter directory
- `--kernelspec-dir DIR`: Custom kernelspec directory
- `--validation LEVEL`: Validation level (none/basic/full)

#### Install Examples

```bash
# Basic user installation
hs-jupyter-kernel install

# System-wide installation with custom display name
hs-jupyter-kernel install --system --display-name "My Haskell Kernel"

# Force reinstall with custom GHC path
hs-jupyter-kernel install --force --ghc-path /opt/ghc/bin/ghc

# Quiet installation for automation
hs-jupyter-kernel install --quiet --json
```

#### Install Success Output

```text
✅ Kernel installation completed successfully!
📁 Kernelspec path: /home/user/.local/share/jupyter/kernels/haskell/kernel.json
🏷️  Display name: Haskell
📦 Version: 0.1.0.0
🔧 GHC path: /usr/bin/ghc
```

### `doctor` - System Diagnostics

Diagnose installation issues and provide troubleshooting recommendations.

```bash
hs-jupyter-kernel doctor [OPTIONS]
```

#### Doctor Options

- `--json`: Output results in JSON format
- `--quiet`: Suppress non-essential output
- `--verbose`: Enable detailed logging
- `--report FILE`: Save detailed diagnostic report to file
- `--check COMPONENT`: Check specific component (jupyter/kernel/ghc/system/all)

#### Doctor Examples

```bash
# Basic system check
hs-jupyter-kernel doctor

# Check only Jupyter installation
hs-jupyter-kernel doctor --check jupyter

# Generate detailed report
hs-jupyter-kernel doctor --report hsjupyter-diagnostics.json

# JSON output for automation
hs-jupyter-kernel doctor --json
```

#### Doctor Sample Output

```text
🔍 System diagnostic completed
📊 Overall status: healthy
⚠️  Issues found: 0
💡 Recommendations: 2

Recommendations:
• Consider updating Jupyter to latest version for best performance
• Enable kernel resource monitoring for production deployments
```

### `list` - List Installations

Show all HsJupyter kernel installations on the system.

```bash
hs-jupyter-kernel list [OPTIONS]
```

#### List Options

- `--json`: Output results in JSON format
- `--quiet`: Suppress non-essential output
- `--all`: Include non-functional installations

#### List Examples

```bash
# List all functional installations
hs-jupyter-kernel list

# Include problematic installations
hs-jupyter-kernel list --all

# JSON output for scripting
hs-jupyter-kernel list --json
```

#### List Sample Output

```text
📋 Found 2 HsJupyter kernel installations

1. User Installation
   📁 Path: /home/user/.local/share/jupyter/kernels/haskell
   📦 Version: 0.1.0.0
   ✅ Status: functional

2. System Installation
   📁 Path: /usr/local/share/jupyter/kernels/haskell
   📦 Version: 0.1.0.0
   ✅ Status: functional
```

### `version` - Version Information

Display version and compatibility information.

```bash
hs-jupyter-kernel version [OPTIONS]
```

#### Version Options

- `--json`: Output results in JSON format
- `--check-compatibility`: Check system compatibility

#### Version Examples

```bash
# Show version information
hs-jupyter-kernel version

# Check compatibility
hs-jupyter-kernel version --check-compatibility

# JSON output
hs-jupyter-kernel version --json
```

#### Version Sample Output

```text
📦 HsJupyter Kernel Version: 0.1.0.0
🏗️  Build info: ghc-9.12.2
✅ System compatibility: OK
```

### `uninstall` - Remove Installation

Uninstall HsJupyter kernel from the system.

```bash
hs-jupyter-kernel uninstall [OPTIONS]
```

#### Uninstall Options

- `--all`: Remove all installations
- `--kernelspec-dir DIR`: Remove from specific directory
- `--confirm`: Skip confirmation prompts
- `--force`: Force removal even with issues
- `--cleanup-all`: Perform global cleanup
- `--remove-config`: Remove configuration files
- `--remove-logs`: Remove log files
- `--json`: Output results in JSON format
- `--quiet`: Suppress non-essential output

#### Uninstall Examples

```bash
# Interactive uninstall (asks for confirmation)
hs-jupyter-kernel uninstall

# Remove all installations without confirmation
hs-jupyter-kernel uninstall --all --confirm

# Force removal with cleanup
hs-jupyter-kernel uninstall --force --cleanup-all

# JSON output for automation
hs-jupyter-kernel uninstall --json
```

#### Uninstall Sample Output

```text
🗑️  Uninstalling HsJupyter kernel...

✅ Removed kernelspec directory: /home/user/.local/share/jupyter/kernels/haskell
✅ Cleaned up temporary files
✅ Cleaned up log files

🎉 Uninstallation completed successfully!
```

## Global Options

All commands support these global options:

- `--help`: Show help information
- `--json`: Output results in JSON format (where applicable)
- `--quiet`: Suppress non-essential output
- `--verbose`: Enable detailed logging

## JSON Output Format

All commands support JSON output for programmatic access using the `--json` flag.

### Install Command JSON

```json
{
  "status": "success",
  "message": "Installation completed successfully",
  "result": {
    "kernelspec_path": "/home/user/.local/share/jupyter/kernels/haskell/kernel.json",
    "display_name": "Haskell",
    "version": "0.1.0.0",
    "ghc_path": "/usr/bin/ghc"
  }
}
```

### Doctor Command JSON

```json
{
  "status": "success",
  "message": "System diagnostic completed",
  "result": {
    "overall_status": "healthy",
    "issues_found": 0,
    "recommendations": 2,
    "issues": [],
    "recommendations": [
      {
        "priority": "medium",
        "action": "Consider updating Jupyter to latest version",
        "rationale": "Newer versions provide better performance and security"
      }
    ]
  }
}
```

## Exit Codes

- `0`: Success
- `1`: General error
- `2`: Invalid command line arguments
- `3`: Installation failed
- `4`: Validation failed
- `5`: Permission denied
- `10`: Jupyter environment not found
- `11`: GHC not found
- `12`: Kernel not functional

## Environment Variables

- `HSJUPYTER_GHC_PATH`: Override default GHC path
- `HSJUPYTER_JUPYTER_DIR`: Override default Jupyter directory
- `JUPYTER_PATH`: Additional Jupyter search paths
- `JUPYTER_DATA_DIR`: Jupyter data directory

## Troubleshooting

See the [CLI Troubleshooting Guide](troubleshooting.md) for common issues and solutions.

## Advanced Usage

### Custom Kernel Configuration

```bash
# Install with custom resource limits
hs-jupyter-kernel install \
  --memory-limit 512 \
  --exec-timeout 30 \
  --display-name "Haskell (Limited)"

# Install with custom environment variables
hs-jupyter-kernel install \
  --env HASKELL_PACKAGES="containers,text" \
  --env OPTIMIZATION_LEVEL="2"
```

### Automation Scripts

```bash
#!/bin/bash
# Automated installation with error handling

if hs-jupyter-kernel install --quiet --json > install_result.json; then
    echo "Installation successful"
    cat install_result.json
else
    echo "Installation failed"
    exit 1
fi
```

### Monitoring and Logging

```bash
# Enable verbose logging
HSJUPYTER_LOG_LEVEL=debug hs-jupyter-kernel install --verbose

# Save diagnostic logs
hs-jupyter-kernel doctor --report diagnostics-$(date +%Y%m%d).json
```
