# Quick Start Guide: Installation & CLI Infrastructure

**Feature**: 004-install-cli  
**Date**: 2025-01-28  
**Audience**: Developers implementing Phase 4 installation commands

## Development Setup

### Prerequisites

Ensure you have the development environment ready:

```bash
# Verify GHC version
ghc --version  # Should be 9.12.2+

# Verify existing HsJupyter kernel builds
cd /home/jjunho/projetos/HsJupyter
cabal build hs-jupyter-kernel

# Verify Jupyter is available for testing
jupyter --version
```

### Project Structure Overview

The CLI extension follows existing constitutional patterns:

```text
src/HsJupyter/
├── CLI/                    # New CLI module tree
│   ├── Commands.hs         # CLI command definitions using optparse-applicative
│   ├── Install.hs          # Installation logic and kernelspec integration
│   ├── Doctor.hs           # Diagnostic and troubleshooting commands
│   ├── Config.hs           # Configuration management and validation
│   └── System.hs           # System integration utilities
├── Runtime/
│   └── ErrorHandling.hs    # Existing shared error handling (integrate with)
└── ...

test/
├── integration/
│   └── CLIIntegrationSpec.hs    # End-to-end workflow tests
└── unit/
    ├── CLICommandsSpec.hs       # Command parsing tests
    ├── InstallSpec.hs           # Installation logic tests
    └── DoctorSpec.hs            # Diagnostic functionality tests
```

## Implementation Workflow

### Step 1: CLI Command Structure

Start with the command parser using `optparse-applicative`:

```haskell
-- src/HsJupyter/CLI/Commands.hs
data CLICommand 
  = Install InstallOptions
  | Doctor DoctorOptions  
  | Uninstall UninstallOptions
  | List ListOptions
  | Version VersionOptions

data InstallOptions = InstallOptions
  { installUser :: Bool
  , installForce :: Bool
  , installQuiet :: Bool
  -- ... other options from spec
  }
```

### Step 2: Constitutional Integration

Integrate with existing error handling and observability:

```haskell
-- Use existing RuntimeDiagnostic system
import HsJupyter.Runtime.ErrorHandling (withErrorContext, enrichDiagnostic)

-- Use existing katip logging
import HsJupyter.Runtime.Telemetry (logStructured, withTelemetryContext)

installKernel :: InstallOptions -> IO (Either CLIDiagnostic KernelInstallation)
installKernel opts = withErrorContext "kernel-installation" $ do
  logStructured InfoS "Starting kernel installation" (toObject opts)
  -- Implementation follows constitutional patterns
```

### Step 3: Test-First Development

Write tests before implementation:

```haskell
-- test/unit/InstallSpec.hs
spec :: Spec
spec = describe "Kernel Installation" $ do
  describe "User Story 1: Jupyter Kernel Installation" $ do
    it "installs kernel successfully with default options" $ do
      -- Test acceptance scenario 1
      
    it "updates existing installation with --force" $ do
      -- Test acceptance scenario 2
      
    it "installs to user directory with --user" $ do
      -- Test acceptance scenario 3
```

## Development Phases

### Phase 1: Foundation (P1 User Story)

Focus on core installation functionality:

1. **CLI parsing**: Implement basic `install` command with essential options
2. **Jupyter detection**: Find kernelspec directories using `directory` library
3. **Kernel registration**: Create and write kernel.json files
4. **Basic validation**: Verify installation can launch kernel

**Success criteria**: `hs-jupyter-kernel install` works on clean system

### Phase 2: Diagnostics (P2 User Story)

Add troubleshooting capabilities:

1. **System scanning**: Detect Jupyter environments and existing installations
2. **Issue identification**: Check for common problems (permissions, paths, versions)
3. **Recommendations**: Provide actionable solutions based on findings
4. **Structured output**: Support `--json` for programmatic use

**Success criteria**: `hs-jupyter-kernel doctor` identifies and suggests fixes for issues

### Phase 3: Advanced Configuration (P3 User Stories)

Add customization and enterprise features:

1. **Custom paths**: Support `--jupyter-dir`, `--kernelspec-dir`, `--ghc-path`
2. **Configuration options**: Custom kernel.json settings, resource limits
3. **Multiple installations**: Handle various Jupyter environments
4. **Programmatic access**: JSON output, quiet mode, exit codes

**Success criteria**: Advanced installation scenarios work reliably

## Testing Strategy

### Unit Tests

Test individual functions and modules:

```bash
# Run CLI module tests
cabal test unit --test-option="--match=/CLI/"

# Run specific command tests  
cabal test unit --test-option="--match=/Install/"
```

### Integration Tests

Test complete workflows:

```bash
# Test end-to-end installation
cabal test integration --test-option="--match=/InstallWorkflow/"

# Test diagnostic workflows
cabal test integration --test-option="--match=/DiagnosticsWorkflow/"
```

### Manual Testing

Use local development workflow:

```bash
# Build with CLI extensions
cabal build hs-jupyter-kernel

# Test installation (be careful with existing setups!)
dist-newstyle/build/x86_64-linux/ghc-9.12.2/hs-jupyter-kernel-0.1.0.0/x/hs-jupyter-kernel/build/hs-jupyter-kernel/hs-jupyter-kernel install --user --dry-run

# Test diagnostics
./hs-jupyter-kernel doctor --json
```

## Key Implementation Notes

### Constitutional Compliance

- **Error Handling**: Use `HsJupyter.Runtime.ErrorHandling` patterns
- **Logging**: Structured logging via katip with correlation IDs
- **Resource Management**: Respect timeout and memory limits
- **Cancellation**: Support TMVar-based cancellation for long operations

### Cross-Platform Considerations

- Use `System.FilePath` for path operations
- Handle Windows/Unix permission differences
- Test on multiple Jupyter installation types (pip, conda, system)

### Performance Targets

- Installation: <2 minutes on standard systems
- Diagnostics: <5 seconds response time
- Memory usage: <100MB during operations
- Disk usage: <100MB temporary files

## Common Pitfalls

1. **Path handling**: Use proper cross-platform path functions
2. **Permissions**: Handle user vs system installation scope correctly
3. **Jupyter variants**: Support both JupyterLab and classic Notebook
4. **Error messages**: Provide actionable, user-friendly error descriptions
5. **State management**: Ensure atomic operations for installation/uninstallation

## Integration Points

### With Existing Kernel

The CLI commands extend the existing kernel executable:

```haskell
-- app/KernelMain.hs (modify existing)
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("install":rest) -> runInstallCommand rest
    ("doctor":rest) -> runDoctorCommand rest
    -- ... other CLI commands
    [] -> runKernelServer -- existing behavior
    _ -> runKernelServer  -- existing behavior
```

### With Constitutional Framework

All CLI functionality integrates with:

- **ResourceGuard**: For operation timeouts and limits
- **RuntimeDiagnostic**: For structured error reporting  
- **Telemetry**: For observability and debugging
- **ErrorHandling**: For DRY error management patterns

## Next Steps

After implementing this specification:

1. Run `/speckit.tasks` to generate detailed implementation tasks
2. Create feature branch following constitutional naming: `004-install-cli`
3. Implement following test-first constitutional principles
4. Update cabal file with new dependencies (`optparse-applicative`)
5. Document any architectural decisions in `docs/architecture.md`
