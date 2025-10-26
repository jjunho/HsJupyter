# CLI API Specification

**Feature**: 004-install-cli  
**Date**: 2025-01-28  
**Version**: 1.0.0

## Command Structure

All CLI commands follow the pattern: `hs-jupyter-kernel <command> [options]`

### Global Options

Available for all commands:

- `--json`: Output structured JSON instead of human-readable text
- `--quiet`: Suppress non-essential output and interactive prompts  
- `--verbose`: Enable detailed logging and diagnostic output
- `--help`: Display command-specific help information

## Command Specifications

### 1. Install Command

**Purpose**: Install or update HsJupyter kernel in Jupyter environment

**Syntax**: `hs-jupyter-kernel install [options]`

**Options**:
- `--user`: Install for current user only (default if no system permissions)
- `--system`: Install system-wide (requires appropriate permissions)
- `--force`: Overwrite existing installation without confirmation
- `--jupyter-dir <path>`: Custom Jupyter configuration directory
- `--kernelspec-dir <path>`: Custom kernelspec installation directory
- `--ghc-path <path>`: Specify custom GHC executable path
- `--display-name <name>`: Custom kernel display name (default: "Haskell")
- `--validation <level>`: Validation level (none|basic|full, default: basic)

**Exit Codes**:
- `0`: Installation successful
- `1`: Installation failed - general error
- `2`: Installation failed - insufficient permissions
- `3`: Installation failed - missing dependencies
- `4`: Installation failed - invalid configuration

**JSON Output Format**:
```json
{
  "status": "success|error",
  "message": "Human readable status message",
  "installation": {
    "kernelspec_path": "/path/to/kernel.json",
    "display_name": "Haskell",
    "version": "0.1.0.0",
    "ghc_path": "/path/to/ghc"
  },
  "validation_results": {
    "kernel_accessible": true,
    "ghc_functional": true,
    "jupyter_integration": true
  },
  "warnings": ["List of non-fatal issues"],
  "recommendations": ["Suggested follow-up actions"]
}
```

### 2. Doctor Command

**Purpose**: Diagnose installation issues and system health

**Syntax**: `hs-jupyter-kernel doctor [options]`

**Options**:
- `--check <component>`: Check specific component (jupyter|kernel|ghc|system|all, default: all)
- `--fix`: Attempt to automatically fix detected issues
- `--report <path>`: Save detailed diagnostic report to file

**Exit Codes**:
- `0`: System healthy or issues successfully resolved
- `1`: Minor issues detected (system functional)
- `2`: Major issues detected (system degraded)
- `3`: Critical issues detected (system broken)

**JSON Output Format**:
```json
{
  "overall_status": "healthy|warnings|degraded|broken",
  "summary": "Brief health assessment",
  "components": {
    "jupyter": {
      "status": "healthy|degraded|broken",
      "version": "4.0.6",
      "kernelspec_dirs": ["/usr/local/share/jupyter/kernels"],
      "writable": true,
      "issues": []
    },
    "kernel": {
      "status": "healthy|degraded|broken|not_installed",
      "version": "0.1.0.0",
      "functional": true,
      "issues": []
    },
    "ghc": {
      "status": "healthy|degraded|broken|not_found",
      "version": "9.12.2", 
      "path": "/usr/local/bin/ghc",
      "issues": []
    },
    "system": {
      "platform": "linux",
      "architecture": "x86_64",
      "shell": "zsh",
      "issues": []
    }
  },
  "issues": [
    {
      "severity": "critical|major|minor|warning",
      "component": "jupyter|kernel|ghc|system",
      "description": "Human readable issue description",
      "details": "Technical details if available"
    }
  ],
  "recommendations": [
    {
      "priority": "immediate|high|medium|low",
      "action": "Recommended action description",
      "command": "hs-jupyter-kernel install --force",
      "rationale": "Why this action helps"
    }
  ]
}
```

### 3. Uninstall Command

**Purpose**: Remove HsJupyter kernel from Jupyter environment

**Syntax**: `hs-jupyter-kernel uninstall [options]`

**Options**:
- `--all`: Remove all HsJupyter kernel installations found
- `--kernelspec-dir <path>`: Remove from specific kernelspec directory
- `--confirm`: Skip confirmation prompts (use with caution)

**Exit Codes**:
- `0`: Uninstallation successful
- `1`: Uninstallation failed - general error
- `2`: Uninstallation failed - insufficient permissions
- `3`: No installations found to remove

**JSON Output Format**:
```json
{
  "status": "success|error|partial",
  "message": "Human readable status message",
  "removed_installations": [
    {
      "kernelspec_path": "/path/to/kernel.json",
      "display_name": "Haskell",
      "success": true
    }
  ],
  "failed_removals": [
    {
      "kernelspec_path": "/path/to/kernel.json",
      "error": "Permission denied"
    }
  ]
}
```

### 4. List Command

**Purpose**: List all HsJupyter kernel installations

**Syntax**: `hs-jupyter-kernel list [options]`

**Options**:
- `--all`: Include non-functional and problematic installations
- `--path <dir>`: Search specific directory for installations

**Exit Codes**:
- `0`: Command executed successfully
- `1`: Error accessing installation directories

**JSON Output Format**:
```json
{
  "installations": [
    {
      "kernelspec_path": "/path/to/kernel.json",
      "display_name": "Haskell",
      "version": "0.1.0.0",
      "status": "installed|issues|corrupted",
      "functional": true,
      "ghc_path": "/usr/local/bin/ghc",
      "install_type": "user|system|conda"
    }
  ],
  "summary": {
    "total_found": 1,
    "functional": 1,
    "with_issues": 0,
    "corrupted": 0
  }
}
```

### 5. Version Command

**Purpose**: Display version information and system compatibility

**Syntax**: `hs-jupyter-kernel version [options]`

**Options**:
- `--check-compatibility`: Check compatibility with current system

**Exit Codes**:
- `0`: Version information displayed successfully
- `1`: Compatibility issues detected

**JSON Output Format**:
```json
{
  "hs_jupyter_kernel": "0.1.0.0",
  "ghc_version": "9.12.2",
  "build_info": {
    "build_date": "2025-01-28",
    "git_hash": "abc123def",
    "platform": "linux-x86_64"
  },
  "compatibility": {
    "jupyter_supported": true,
    "ghc_compatible": true,
    "platform_supported": true,
    "issues": []
  }
}
```

## Error Handling

All commands implement constitutional error handling patterns:

### Error Response Format (JSON mode)

```json
{
  "status": "error",
  "error": {
    "type": "InstallationError|ValidationError|ConfigurationError|SystemIntegrationError",
    "message": "User-friendly error description", 
    "details": "Technical details for troubleshooting",
    "suggestions": ["Possible solutions"],
    "exit_code": 1
  },
  "context": {
    "command": "install",
    "options": ["--user", "--force"],
    "system_info": "Relevant system details"
  }
}
```

### Constitutional Compliance

- All operations respect resource limits (timeout, memory)
- Structured logging via katip for all operations
- Cancellation support through TMVar patterns
- Defensive programming with input validation
- Law of Demeter compliance in module interactions

## Testing Contracts

### Unit Test Requirements

Each command MUST have corresponding test cases that verify:

1. **Success scenarios**: All documented options and workflows
2. **Error scenarios**: All documented exit codes and error conditions  
3. **Edge cases**: Invalid inputs, missing dependencies, permission issues
4. **JSON output**: All documented JSON structures and fields
5. **Constitutional compliance**: Resource limits, cancellation, logging

### Integration Test Requirements

Full workflow tests that verify:

1. **End-to-end installation**: From clean system to functional kernel
2. **Cross-platform compatibility**: Linux, macOS, Windows
3. **Jupyter integration**: Kernel appears and functions in Jupyter
4. **Error recovery**: System recovers gracefully from failures
5. **Performance targets**: Commands meet specified time limits

### Acceptance Test Mapping

Each user story acceptance scenario becomes an automated test:

- **User Story 1**: `test/integration/InstallWorkflowSpec.hs`
- **User Story 2**: `test/integration/DiagnosticsWorkflowSpec.hs`  
- **User Story 3**: `test/integration/CustomConfigurationSpec.hs`
- **User Story 4**: `test/integration/SystemIntegrationSpec.hs`