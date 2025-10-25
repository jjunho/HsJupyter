# JSON API Schema

**Feature**: 004-install-cli  
**Date**: 2025-01-28

## Command Response Schema

### Install Command Response

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "status": {
      "type": "string",
      "enum": ["success", "error"]
    },
    "message": {
      "type": "string",
      "description": "Human readable status message"
    },
    "installation": {
      "type": "object",
      "properties": {
        "kernelspec_path": {"type": "string"},
        "display_name": {"type": "string"},
        "version": {"type": "string"},
        "ghc_path": {"type": "string"}
      },
      "required": ["kernelspec_path", "display_name", "version", "ghc_path"]
    },
    "validation_results": {
      "type": "object",
      "properties": {
        "kernel_accessible": {"type": "boolean"},
        "ghc_functional": {"type": "boolean"},
        "jupyter_integration": {"type": "boolean"}
      }
    },
    "warnings": {
      "type": "array",
      "items": {"type": "string"}
    },
    "recommendations": {
      "type": "array",
      "items": {"type": "string"}
    }
  },
  "required": ["status", "message"]
}
```

### Doctor Command Response

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "overall_status": {
      "type": "string",
      "enum": ["healthy", "warnings", "degraded", "broken"]
    },
    "summary": {"type": "string"},
    "components": {
      "type": "object",
      "properties": {
        "jupyter": {
          "type": "object",
          "properties": {
            "status": {"type": "string"},
            "version": {"type": "string"},
            "kernelspec_dirs": {
              "type": "array",
              "items": {"type": "string"}
            },
            "writable": {"type": "boolean"},
            "issues": {
              "type": "array",
              "items": {"type": "string"}
            }
          }
        },
        "kernel": {
          "type": "object",
          "properties": {
            "status": {"type": "string"},
            "version": {"type": "string"},
            "functional": {"type": "boolean"},
            "issues": {
              "type": "array",
              "items": {"type": "string"}
            }
          }
        }
      }
    },
    "issues": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "severity": {
            "type": "string",
            "enum": ["critical", "major", "minor", "warning"]
          },
          "component": {
            "type": "string",
            "enum": ["jupyter", "kernel", "ghc", "system"]
          },
          "description": {"type": "string"},
          "details": {"type": "string"}
        }
      }
    },
    "recommendations": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "priority": {
            "type": "string",
            "enum": ["immediate", "high", "medium", "low"]
          },
          "action": {"type": "string"},
          "command": {"type": "string"},
          "rationale": {"type": "string"}
        }
      }
    }
  },
  "required": ["overall_status", "summary"]
}
```

### Error Response Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "status": {
      "type": "string",
      "const": "error"
    },
    "error": {
      "type": "object",
      "properties": {
        "type": {
          "type": "string",
          "enum": [
            "InstallationError",
            "ValidationError", 
            "ConfigurationError",
            "SystemIntegrationError"
          ]
        },
        "message": {"type": "string"},
        "details": {"type": "string"},
        "suggestions": {
          "type": "array",
          "items": {"type": "string"}
        },
        "exit_code": {"type": "integer"}
      },
      "required": ["type", "message", "exit_code"]
    }
  },
  "required": ["status", "error"]
}
```