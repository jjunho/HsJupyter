# Research: Jupyter Kernel Integration Bug Fix

This document details the research conducted to address the bug preventing the HsJupyter kernel from integrating with Jupyter environments.

## 1. Performance Targets

- **Decision**:
  - **Kernel Startup**: The kernel should be ready to execute code within 5 seconds of a user opening a new notebook.
  - **Code Execution**: Simple expressions (e.g., `1 + 1`) should return output in under 500ms.
- **Rationale**: These targets are aligned with user expectations for an interactive environment and are comparable to the performance of other Jupyter kernels (e.g., IJulia, IRKernel). They provide a responsive user experience without being overly demanding for a first-pass implementation.
- **Alternatives Considered**:
  - **Stricter Targets (<2s startup, <200ms execution)**: While desirable, these targets might require significant optimization work that is out of scope for this bug fix. The current focus is on functionality.
  - **No Targets**: This was rejected as it would make it difficult to validate the quality of the fix and could lead to a poor user experience.

## 2. Jupyter Messaging Protocol

- **Decision**: The kernel will be updated to fully comply with version 5.3 of the Jupyter messaging protocol. This includes:
  - Correctly handling all required message types (`kernel_info_request`, `execute_request`, `shutdown_request`, `interrupt_request`).
  - Ensuring all message headers and content are correctly formatted.
  - Properly signing messages with HMAC-SHA256 if a session key is provided.
- **Rationale**: The most likely cause of the integration bug is a mismatch or incorrect implementation of the Jupyter messaging protocol. Adhering strictly to a well-defined version of the protocol is the most direct path to a robust solution. Version 5.3 is the most widely supported version by current Jupyter clients.
- **Alternatives Considered**:
  - **Protocol Version 5.4+**: Newer versions of the protocol exist, but they are not as universally supported by all Jupyter clients in the wild. Targeting 5.3 ensures maximum compatibility.
  - **Ad-hoc Fixes**: Attempting to patch the existing implementation without a full protocol review would likely lead to a brittle solution that fails with different Jupyter clients or future updates.

## 3. Kernel Lifecycle Management

- **Decision**: The kernel's main loop will be refactored to handle the full lifecycle of a Jupyter kernel:
  1. **Startup**: Bind to the ZMQ sockets specified in the connection file.
  2. **Heartbeat**: Respond to heartbeat pings on the HB socket to signal that the kernel is alive.
  3. **Message Handling**: Process requests from the shell, control, and stdin sockets.
  4. **Shutdown**: Cleanly exit upon receiving a `shutdown_request`.
  5. **Interrupt**: Handle `interrupt_request` messages to gracefully stop ongoing computations.
- **Rationale**: A robust lifecycle management implementation is critical for stability. The current bug is likely related to a failure in one of these lifecycle stages (e.g., not responding to heartbeats, mishandling shutdown requests).
- **Alternatives Considered**:
  - **Ignoring certain channels (e.g., control)**: While some channels are less frequently used, ignoring them entirely violates the protocol and can lead to instability, especially with more advanced clients like JupyterLab.
