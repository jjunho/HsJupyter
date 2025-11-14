# Feature Specification: Jupyter Kernel Integration Bug Fix

**Feature Branch**: `005-fix-jupyter-kernel-bug`
**Created**: 2025-11-04
**Status**: Implemented (Documentation Retroactive - 248 passing tests, integration tests pending validation)
**Input**: Fix critical bug preventing HsJupyter kernel from integrating with Jupyter Notebook and JupyterLab environments

## User Scenarios & Testing *(mandatory)*

> Align every user story with the constitution: document-first narrative, explicit pre-code tests, and observability hooks. Each story MUST describe how logging/metrics/diagnostics validate the behaviour.

### User Story 1 - Execute Code and Receive Output (Priority: P1) 🎯 MVP

A data scientist working in Jupyter Notebook opens a new Haskell notebook, types a simple Haskell expression like `1 + 1` or `putStrLn "Hello, Jupyter!"`, executes the cell, and immediately sees the correct output displayed below the cell. The kernel status indicator shows "Kernel Ready" and execution completes within 500ms for simple expressions.

**Why this priority**: This is the absolute minimum functionality required for the kernel to be usable. Without the ability to execute code and see output, the kernel provides no value. This represents the core contract of any Jupyter kernel: accept code input, execute it, and return results.

**Independent Test**: Can be fully tested by launching Jupyter Notebook, creating a new Haskell notebook, executing a cell with `1 + 1`, and verifying the output `2` is displayed. This delivers immediate value as a working REPL environment within Jupyter.

**Observability**: The kernel MUST emit structured logs via `katip` for each execution request, including:
- Execution start time and message ID
- Code being executed (truncated if >1KB)
- Execution completion status (success/failure)
- Total execution time
- Output size and type (stdout, stderr, result)

**Acceptance Scenarios**:

1. **Given** a fresh Haskell notebook is open, **When** user executes a cell containing `1 + 1`, **Then** the output `2` is displayed below the cell within 500ms
2. **Given** the kernel is ready, **When** user executes `putStrLn "Hello, Jupyter!"`, **Then** the text `Hello, Jupyter!` appears in the output area as a stream message
3. **Given** the kernel is ready, **When** user executes code that writes to stderr (e.g., `hPutStrLn stderr "Warning"`), **Then** the warning message appears in the output with appropriate formatting
4. **Given** the kernel is executing code, **When** the code produces both stdout and a result value, **Then** both are displayed in the correct order
5. **Given** structured logging is enabled, **When** any execution occurs, **Then** logs include message ID, execution time, and status

---

### User Story 2 - Handle Kernel Interruptions and Restarts (Priority: P2)

A user accidentally starts an infinite loop or long-running computation in their notebook. They click the "Interrupt" button in the Jupyter UI, and the kernel immediately stops the execution, returning to a ready state. If needed, they can also restart the kernel completely, which clears all session state and makes the kernel ready for fresh commands.

**Why this priority**: This is critical for usability but not blocking for basic functionality. Users need the ability to recover from mistakes (infinite loops, runaway computations) without restarting their entire Jupyter session. This is a standard feature of all mature Jupyter kernels.

**Independent Test**: Execute a cell with an infinite loop (`let loop = loop in loop`), click the Interrupt button, and verify the kernel becomes responsive again. Then use Restart to clear kernel state and verify variables are forgotten.

**Observability**: The kernel MUST emit structured logs for:
- Interrupt requests received with target message ID
- Successful/failed interrupt attempts
- Shutdown requests with restart flag status
- Kernel restart completion time

**Acceptance Scenarios**:

1. **Given** the kernel is executing an infinite loop, **When** user clicks "Interrupt Kernel" in Jupyter UI, **Then** the execution stops within 1 second and kernel returns to ready state
2. **Given** variables are defined in the kernel session, **When** user clicks "Restart Kernel", **Then** the kernel restarts and previously defined variables are no longer accessible
3. **Given** the kernel is idle, **When** user requests restart, **Then** the kernel completes restart within 5 seconds and is ready for new commands
4. **Given** an interrupt is issued during execution, **When** the kernel stops, **Then** structured logs include the interrupt request ID and completion status
5. **Given** a shutdown request with restart=false, **When** the kernel processes it, **Then** the kernel exits cleanly with exit code 0

---

### Edge Cases

- **What happens when the connection file is missing or malformed?** The kernel MUST fail fast on startup with a clear error message (exit code 1) and log the specific validation error (e.g., "Config file missing at /path", "Invalid port number", "Unsupported signature scheme").

- **How does the system handle malformed Jupyter protocol messages?** The kernel MUST log the decode error with the invalid message type, send an error reply to the client, and continue running (graceful degradation). The kernel MUST NOT crash.

- **What happens when code execution exceeds resource limits?** The kernel MUST enforce the configured CPU timeout (default 10s) and memory limit (default 512MB) via ResourceGuard, terminate the execution, and send an error message with diagnostic information.

- **How does the kernel handle concurrent execute requests?** Execute requests are queued in the RuntimeManager with a capacity of 16 jobs. Requests beyond capacity are rejected with a queue full error. Only one execution runs at a time per kernel.

- **What happens when HMAC signature verification fails?** If a signature scheme is configured (hmac-sha256) and a message fails verification, the kernel MUST log a security warning, drop the message, and continue running without executing the code.

- **How does the kernel handle messages on unsupported message types?** The kernel MUST respond with a DecodeFailure error indicating "Unsupported message type: X" and log the occurrence without crashing.

- **What happens during heartbeat failure?** The heartbeat runs on a dedicated socket (ZMQ Rep pattern). If heartbeat messages stop arriving, the kernel continues running but may be marked as unresponsive by Jupyter. The heartbeat thread is independent of execution.

## Requirements *(mandatory)*

### Functional Requirements

> Cover runtime safety obligations (resource guards, cancellation), test coverage, and observability expectations alongside feature behaviour.

#### Protocol Compliance

- **FR-001**: Kernel MUST implement Jupyter messaging protocol version 5.3, supporting all required message types: `kernel_info_request`, `execute_request`, `shutdown_request`, and `interrupt_request`
- **FR-002**: Kernel MUST correctly format all message envelopes with headers (msg_id, session, username, msg_type, version, date), parent headers, metadata, and content
- **FR-003**: Kernel MUST sign all outgoing messages with HMAC-SHA256 when a session key is provided in the connection file
- **FR-004**: Kernel MUST verify HMAC-SHA256 signatures on incoming messages when signature scheme is configured

#### Kernel Lifecycle

- **FR-005**: Kernel MUST bind to all five ZMQ sockets specified in the connection file: shell (Router), control (Router), stdin (Router), iopub (Pub), heartbeat (Rep)
- **FR-006**: Kernel MUST respond to heartbeat pings on the HB socket within 100ms to signal liveness
- **FR-007**: Kernel MUST complete startup and be ready to process messages within 5 seconds of launch (see research.md:8, plan.md:20)
- **FR-008**: Kernel MUST handle `shutdown_request` messages and exit cleanly with exit code 0, optionally supporting restart flag
- **FR-009**: Kernel MUST respond to `kernel_info_request` with correct language metadata (name: "haskell", version, mimetype: "text/x-haskell", file_extension: ".hs")

#### Code Execution

- **FR-010**: Kernel MUST execute Haskell code submitted via `execute_request` messages using the GHC runtime API
- **FR-011**: Kernel MUST capture stdout output during execution and send it as `stream` messages on the iopub socket
- **FR-012**: Kernel MUST capture stderr output during execution and send it as separate `stream` messages with stream name "stderr"
- **FR-013**: Kernel MUST send the final result value (if any) as an `execute_result` message with execution count
- **FR-014**: Kernel MUST send an immediate `execute_reply` on the shell socket acknowledging the request
- **FR-015**: Kernel MUST execute simple expressions (e.g., `1 + 1`) and return results within 500ms (see research.md:9, plan.md:20)
- **FR-016**: Kernel MUST support the `silent` flag in execute requests to suppress output
- **FR-017**: Kernel MUST support the `store_history` flag to control execution count incrementing

#### Interruption & Control

- **FR-018**: Kernel MUST handle `interrupt_request` messages and gracefully terminate the currently running computation
- **FR-019**: Kernel MUST use STM or async exceptions to implement interruption in the GHC runtime
- **FR-020**: Kernel MUST respond to interrupt requests within 1 second with an `interrupt_reply` message (see research.md:33)
- **FR-021**: Kernel MUST return to ready state after successful interruption, allowing new execute requests

#### Resource Management

- **FR-022**: Kernel MUST enforce a CPU timeout limit (default: 10 seconds) on code execution via ResourceGuard (see data-model.md, KernelProcess.hs:79)
- **FR-023**: Kernel MUST enforce a memory limit (default: 512MB) on code execution via ResourceGuard (see data-model.md, KernelProcess.hs:80)
- **FR-024**: Kernel MUST enforce a maximum output size limit (default: payload limit bytes) to prevent excessive memory usage
- **FR-025**: Kernel MUST terminate execution that exceeds resource limits and send error diagnostics

#### Observability & Diagnostics

- **FR-026**: Kernel MUST emit structured telemetry for all execution requests including message ID, execution time, status, and output size
- **FR-027**: Kernel MUST log all protocol errors (decode failures, signature failures) with severity classification
- **FR-028**: Kernel MUST log lifecycle events (startup, shutdown, restart) with timestamps
- **FR-029**: Kernel MUST use `katip` for structured JSON logging with correlation IDs matching Jupyter message IDs
- **FR-030**: Kernel MUST provide diagnostic information in error messages including error type, context, and suggested remediation

#### Testing & Quality

- **FR-031**: All handlers (kernel_info, execute, interrupt, shutdown) MUST have unit tests with 100% coverage
- **FR-032**: Integration tests MUST validate end-to-end execution workflow with real ZMQ sockets
- **FR-033**: Tests MUST validate protocol message format compliance with Jupyter specification
- **FR-034**: Performance tests MUST verify startup time <5s and execution time <500ms for simple expressions

### Key Entities *(internal)*

- **Message**: Represents a Jupyter protocol message with header (msg_id, session, username, msg_type, version, date), parent_header, metadata, and content. All kernel communication uses this structure.

- **ConnectionInfo**: Contains configuration from connection.json file including IP address, port numbers for all five sockets (shell, control, stdin, iopub, heartbeat), transport protocol (tcp), signature scheme (hmac-sha256 or empty), and session key for message signing.

- **KernelState**: In-memory state including the GHC session for code evaluation, execution count, user-defined variables, and job queue. State persists across executions but is cleared on restart.

- **ExecuteContext**: Metadata for an execution request including message ID, session ID, username, parent message ID. Used for correlation in logs and responses.

- **JobMetadata**: Execution options from execute_request including silent flag, store_history flag, allow_stdin flag, and user_expressions dictionary.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can successfully install the HsJupyter kernel using `jupyter kernelspec list` and see it appear in the available kernels list

- **SC-002**: Users can create a new Haskell notebook in Jupyter Notebook or JupyterLab and the kernel status indicator shows "Kernel Ready" within 5 seconds (see FR-007)

- **SC-003**: Users can execute simple Haskell expressions (e.g., `1 + 1`, `2 * 21`) and see correct output displayed within 500ms with 100% accuracy (see FR-015)

- **SC-004**: Users can execute code that produces stdout/stderr output (e.g., `putStrLn "Hello"`) and see the output correctly displayed in the notebook

- **SC-005**: Users can interrupt long-running or infinite loop computations using the Jupyter UI interrupt button with 100% success rate within 1 second (see FR-020)

- **SC-006**: Users can restart the kernel and verify that previous session state is cleared (variables are forgotten) with 100% reliability

- **SC-007**: The kernel remains stable through 100 consecutive execute-interrupt-execute cycles without crashes or hangs

- **SC-008**: All unit tests pass (248 examples, 0 failures) and integration tests validate end-to-end workflows with real Jupyter clients

- **SC-009**: Structured logs contain complete execution traces with message IDs, timings, and status for all operations, enabling debugging of issues

- **SC-010**: The kernel correctly handles malformed messages by logging errors and sending error replies without crashing, maintaining 100% uptime under protocol violations
