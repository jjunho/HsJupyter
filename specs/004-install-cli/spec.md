# Feature Specification: Installation & CLI Infrastructure

**Feature Branch**: `004-install-cli`  
**Created**: 2025-01-28  
**Status**: Draft  
**Input**: User description: "Installation & CLI Infrastructure"

## User Scenarios & Testing *(mandatory)*

> Align every user story with the constitution: document-first narrative, explicit pre-code tests, and observability hooks. Each story MUST describe how logging/metrics/diagnostics validate the behaviour.

<!--
  IMPORTANT: User stories should be PRIORITIZED as user journeys ordered by importance.
  Each user story/journey must be INDEPENDENTLY TESTABLE - meaning if you implement just ONE of them,
  you should still have a viable MVP (Minimum Viable Product) that delivers value.
  
  Assign priorities (P1, P2, P3, etc.) to each story, where P1 is the most critical.
  Think of each story as a standalone slice of functionality that can be:
  - Developed independently
  - Tested independently
  - Deployed independently
  - Demonstrated to users independently
-->

### User Story 1 - Jupyter Kernel Installation (Priority: P1)

Data scientists and Haskell developers want to easily install the HsJupyter kernel into their existing Jupyter environment without complex manual configuration steps.

**Why this priority**: This is the primary entry point for all users. Without simple installation, the kernel cannot be used regardless of its functionality.

**Independent Test**: Can be fully tested by running `hs-jupyter-kernel install` on a clean system with Jupyter installed and verifying the kernel appears in `jupyter kernelspec list` and launches successfully.

**Acceptance Scenarios**:

1. **Given** a system with Jupyter installed but no HsJupyter kernel, **When** user runs `hs-jupyter-kernel install`, **Then** the kernel is registered and available in Jupyter Lab/Notebook
2. **Given** an existing HsJupyter kernel installation, **When** user runs `hs-jupyter-kernel install --force`, **Then** the kernel is updated to the new version
3. **Given** insufficient permissions for system-wide installation, **When** user runs `hs-jupyter-kernel install --user`, **Then** the kernel is installed in user-specific location

---

### User Story 2 - Installation Diagnostics and Troubleshooting (Priority: P2)

Users experiencing installation issues need clear diagnostic information and guided troubleshooting to resolve common problems without manual debugging.

**Why this priority**: Reduces support burden and improves user experience by enabling self-service troubleshooting.

**Independent Test**: Can be tested by running `hs-jupyter-kernel doctor` on systems with various configuration issues and verifying helpful diagnostic output.

**Acceptance Scenarios**:

1. **Given** a system with installation issues, **When** user runs `hs-jupyter-kernel doctor`, **Then** specific problems are identified with actionable resolution steps
2. **Given** a correctly installed kernel, **When** user runs `hs-jupyter-kernel doctor`, **Then** system reports healthy status with version information
3. **Given** missing dependencies, **When** user runs `hs-jupyter-kernel doctor`, **Then** missing components are identified with installation instructions

---

### User Story 3 - Custom Installation Configuration (Priority: P3)

Advanced users need to customize installation paths, kernel configurations, and integration settings for specific deployment environments.

**Why this priority**: Enables enterprise deployments and custom environments while not blocking basic usage.

**Independent Test**: Can be tested by installing with custom configuration options and verifying the kernel operates with the specified settings.

**Acceptance Scenarios**:

1. **Given** a custom Jupyter environment, **When** user specifies custom paths via `--jupyter-dir` and `--kernel-dir`, **Then** kernel is installed in specified locations
2. **Given** need for specific kernel configuration, **When** user provides custom kernel.json settings, **Then** kernel operates with specified resource limits and display name
3. **Given** multiple Haskell versions, **When** user specifies `--ghc-path`, **Then** kernel uses the specified GHC installation

---

### User Story 4 - System Integration and Verification (Priority: P3)

System administrators and CI/CD pipelines need programmatic installation verification and integration with package managers.

**Why this priority**: Enables automated deployments and enterprise integration scenarios.

**Independent Test**: Can be tested by running installation commands in automated environments and verifying JSON/structured output.

**Acceptance Scenarios**:

1. **Given** a CI/CD pipeline, **When** installation commands are run with `--json` flag, **Then** structured output enables programmatic verification
2. **Given** need for silent installation, **When** user runs commands with `--quiet` flag, **Then** installation proceeds without interactive prompts
3. **Given** multiple kernel versions, **When** user runs `hs-jupyter-kernel list`, **Then** all installed versions are displayed with status information

### Edge Cases

- What happens when Jupyter is not installed or not in PATH?
- How does system handle corrupted existing kernel installations?
- What occurs when insufficient disk space or permissions prevent installation?
- How does the system handle network connectivity issues during installation verification?
- What happens when GHC/Haskell dependencies are missing or incompatible versions?
- How does the system behave when multiple Jupyter environments exist (conda, pip, system)?

## Requirements *(mandatory)*

<!--
  ACTION REQUIRED: The content in this section represents placeholders.
  Fill them out with the right functional requirements.
-->

### Functional Requirements

> Cover runtime safety obligations (resource guards, cancellation), test coverage, and observability expectations alongside feature behaviour.

- **FR-001**: System MUST provide `hs-jupyter-kernel install` command that registers the kernel with Jupyter
- **FR-002**: System MUST detect existing Jupyter installations and integrate with standard kernelspec directories
- **FR-003**: System MUST validate GHC and Haskell toolchain availability before installation
- **FR-004**: System MUST provide `hs-jupyter-kernel doctor` command for installation diagnostics
- **FR-005**: System MUST support both user-specific (`--user`) and system-wide installation modes
- **FR-006**: System MUST generate valid kernel.json configuration with appropriate resource limits
- **FR-007**: System MUST provide `hs-jupyter-kernel uninstall` command for clean removal
- **FR-008**: System MUST support custom installation paths via command-line options
- **FR-009**: System MUST verify kernel functionality after installation with basic evaluation test
- **FR-010**: System MUST emit structured logs for all installation operations via existing katip system
- **FR-011**: System MUST define resource guard thresholds for installation operations (timeout, disk space)
- **FR-012**: System MUST support force reinstallation to update existing installations
- **FR-013**: System MUST provide version information and compatibility checking
- **FR-014**: System MUST handle installation cancellation gracefully via TMVar-based cancellation
- **FR-015**: System MUST integrate with existing constitutional error handling patterns

### Key Entities

- **Kernel Installation**: Represents a configured HsJupyter kernel installation with path, version, and configuration status
- **Jupyter Environment**: Represents detected Jupyter installation with kernelspec directories and python environment information
- **Installation Configuration**: Represents user-specified installation parameters including paths, options, and resource limits
- **Diagnostic Result**: Represents health check outcomes with status, issues found, and recommended actions

## Success Criteria *(mandatory)*

<!--
  ACTION REQUIRED: Define measurable success criteria.
  These must be technology-agnostic and measurable.
-->

### Measurable Outcomes

- **SC-001**: Users can install HsJupyter kernel in under 2 minutes on standard systems with Jupyter pre-installed
- **SC-002**: Installation success rate exceeds 95% on supported platforms (Linux, macOS, Windows with standard Jupyter setups)
- **SC-003**: 90% of installation issues are resolved through `doctor` command without manual intervention
- **SC-004**: Installation process completes within 30 seconds on systems with all dependencies available
- **SC-005**: Diagnostic command identifies and reports specific issues in under 5 seconds
- **SC-006**: Kernel appears in Jupyter interface within 10 seconds of successful installation
- **SC-007**: Uninstallation removes all kernel files and registry entries with 100% success rate
- **SC-008**: Installation process uses less than 100MB of temporary disk space during operation

## Assumptions

- Jupyter (Lab or Notebook) is already installed on target systems
- GHC 9.12.2+ and Cabal are available in system PATH
- Standard Python package management tools (pip/conda) are functional
- Users have appropriate permissions for their chosen installation scope
- Network connectivity is available for dependency verification
- Existing HsJupyter kernel installation detection can rely on standard kernelspec locations

## Dependencies

- **External**: Jupyter ecosystem (kernelspec command, standard directory structures)
- **Internal**: Existing HsJupyter kernel executable with constitutional compliance
- **Technical**: GHC/Haskell toolchain for kernel operation verification
- **Process**: Constitutional principles for error handling, observability, and resource management

## Scope

### In Scope

- Command-line installation and management tools
- Jupyter kernelspec integration and registration
- Installation diagnostics and troubleshooting
- Custom configuration and path handling
- Basic kernel functionality verification
- Integration with existing constitutional framework

### Out of Scope

- Package manager integration (apt, brew, chocolatey) - future enhancement
- GUI installation tools - command-line only
- Automatic dependency installation (GHC, Cabal) - user responsibility
- Advanced kernel configuration management - basic kernel.json only
- Multi-user system administration tools - individual installation focus
