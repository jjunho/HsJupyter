# Configuration Reference (Draft)

This document will capture every configurable option for HsJupyter once the kernel implementation is in place. Use it to explain precedence rules, defaults, and recommended values.

## Planned Sections

- **Configuration Sources**
  - Built-in defaults
  - System-level overrides (`/etc/hsjupyter/config.toml`)
  - User-level configuration (`~/.config/hsjupyter/config.toml`)
  - Notebook metadata
  - Environment variables (`HSJUPYTER_*`)
- **Global Options**
  - Logging and telemetry
  - Runtime selection and sandboxing
  - Resource limits (memory, timeouts)
  - Cache directories
- **Capability Toggles**
  - Completions
  - Diagnostics
  - Widgets
  - Experimental features
- **Schema Definition**
  - TOML schema table
  - Validation rules and error messages

> TODO: Fill in concrete keys, types, defaults, and usage examples as the configuration layer is implemented.
