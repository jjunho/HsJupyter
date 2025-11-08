# Building a Native Jupyter Kernel in Haskell

This guide provides architectural context for HsJupyter's implementation as a native Jupyter kernel written in Haskell.

## Core Concepts of a Jupyter Kernel

A Jupyter kernel is an independent process that executes code for a specific language. The Jupyter frontend (Notebook or JupyterLab) communicates with the kernel via a standardized protocol, but does not execute code itself.

### Discovery and Configuration

- **`kernel.json`**: Jupyter discovers kernels by searching for this file in specific directories. It tells Jupyter how to start the kernel.
- **Connection File**: When starting a kernel, Jupyter provides a JSON file path as a command-line argument containing IP addresses, ports, and a secret key for communication.
- **Communication Protocol**: All communication happens over ZeroMQ sockets using the Jupyter Messaging Protocol with HMAC-SHA256 signed multipart messages containing JSON payloads.

## HsJupyter's kernel.json File

The kernel specification file for HsJupyter:

```json
{
  "argv": [
    "/path/to/hs-jupyter-kernel",
    "--connection",
    "{connection_file}"
  ],
  "display_name": "Haskell",
  "language": "haskell"
}
```

- `argv`: Command to execute. `{connection_file}` is replaced by Jupyter with the actual path.
- `display_name`: User-friendly name shown in Jupyter's UI.
- `language`: Language identifier for syntax highlighting.

**Installation**: Place in `~/.local/share/jupyter/kernels/hsjupyter/kernel.json` (or equivalent for your platform). Use `jupyter kernelspec list` to verify.

## HsJupyter Architecture

HsJupyter implements the kernel natively in Haskell with the following components:

### 1. Connection File Parsing

Entry point in `app/KernelMain.hs` reads the connection file and parses it using `aeson`:

```haskell
data ConnectionInfo = ConnectionInfo {
  ip :: String,
  transport :: String,
  shell_port :: Int,
  control_port :: Int,
  stdin_port :: Int,
  hb_port :: Int,
  iopub_port :: Int,
  key :: String
} deriving (Show, Generic)
```

### 2. ZeroMQ Socket Initialization

The kernel establishes five required sockets (implemented in `HsJupyter.KernelProcess`):

- **Heartbeat (hb_port)**: `REP` socket responding to pings, runs in separate thread (`HsJupyter.Bridge.HeartbeatThread`)
- **Shell (shell_port)**: `ROUTER` socket for execution requests and queries
- **Control (control_port)**: `ROUTER` socket for out-of-band messages (shutdown, interrupt)
- **Stdin (stdin_port)**: `ROUTER` socket for standard input requests
- **IOPub (iopub_port)**: `PUB` socket for broadcasting outputs, status, and results

### 3. Message Protocol Implementation

Implemented in `HsJupyter.Bridge.Protocol.{Envelope,Codec}`:

**Jupyter Message Structure** (multipart `ByteString` sequence):

1. `<IDS|DELIM>`: Routing identities followed by delimiter
2. `SIGNATURE`: HMAC-SHA256 signature
3. `HEADER`: JSON with message ID, type, protocol version
4. `PARENT_HEADER`: Header of initiating message
5. `METADATA`: JSON metadata dictionary
6. `CONTENT`: JSON payload (e.g., code to execute)

**Message Handling Workflow:**

1. Receive multipart message from socket
2. Verify HMAC-SHA256 signature using connection key
3. Decode JSON payloads into Haskell types
4. Dispatch based on `msg_type` field in header

### 4. Core Message Handlers

Implemented in `HsJupyter.Router.RequestRouter` and `HsJupyter.Bridge.JupyterBridge`:

#### kernel_info_request

Returns kernel metadata and capabilities:

```json
{
  "protocol_version": "5.3",
  "implementation": "hsjupyter",
  "implementation_version": "0.1.0",
  "language_info": {
    "name": "haskell",
    "version": "9.6.7",
    "mimetype": "text/x-haskell",
    "file_extension": ".hs"
  },
  "banner": "HsJupyter - A Haskell Kernel for Jupyter"
}
```

#### execute_request

Executes user code through the GHC runtime (implemented in `HsJupyter.Runtime.*`):

1. Publish `busy` status on IOPub
2. Extract code from request content
3. Execute via persistent GHC session (`HsJupyter.Runtime.GHCSession`, `HsJupyter.Runtime.GHCRuntime`)
4. Broadcast outputs:
   - `stream` messages for stdout/stderr
   - `error` messages for exceptions with tracebacks
   - `execute_result` for expression results
5. Send `execute_reply` on Shell socket
6. Publish `idle` status on IOPub

## HsJupyter Implementation Details

### Main Thread (`HsJupyter.KernelProcess`)

- Parses connection file
- Launches heartbeat thread
- Sets up all ZeroMQ sockets
- Enters main message-handling loop
- Coordinates shutdown and cleanup

### Heartbeat Thread (`HsJupyter.Bridge.HeartbeatThread`)

- Listens on heartbeat socket
- Echoes back any received data immediately
- Runs independently in separate thread

### GHC Evaluation Engine (`HsJupyter.Runtime.*`)

- **GHCSession**: Manages persistent hint interpreter state
- **GHCRuntime**: Core evaluation functions with cancellation and resource monitoring
- **Evaluation**: High-level evaluation interface
- **SessionState**: Maintains bindings, imports, execution count
- **ResourceGuard**: Enforces memory/CPU/output limits
- **Diagnostics**: Error reporting with actionable suggestions
- **Telemetry**: Performance monitoring and metrics collection

### Message Construction (`HsJupyter.Bridge.Protocol.*`)

Helper modules for building and signing Jupyter-compliant messages:

- **Envelope**: Typed message structures
- **Codec**: Encoding/decoding between wire format and Haskell types
- HMAC-SHA256 signature generation and verification

## Implementation Status

**Completed:**

- ✅ Full ZeroMQ protocol implementation
- ✅ HMAC signature validation
- ✅ All required message types (kernel_info, execute, interrupt, shutdown)
- ✅ Persistent GHC session with hint library
- ✅ Resource management and cancellation
- ✅ Comprehensive error handling and diagnostics
- ✅ Session state persistence
- ✅ Structured logging and telemetry

**In Development:**

- 🔄 CLI installation tools
- 🔄 Diagnostic utilities

**Planned:**

- 📋 Code completion
- 📋 Symbol inspection
- 📋 Interactive debugging
- 📋 Rich output renderers

## Key Dependencies

- **zeromq4-haskell**: ZeroMQ socket communication
- **aeson**: JSON parsing and encoding
- **hint**: GHC API integration for code evaluation
- **cryptonite**: HMAC-SHA256 message signing
- **stm**: Software Transactional Memory for concurrency
- **katip**: Structured logging
- **bytestring**, **text**: Data handling
- **uuid**: Message ID generation

## References

- Jupyter Messaging Protocol: <https://jupyter-client.readthedocs.io/en/stable/messaging.html>
- Kernel Connection Files: <https://jupyter-client.readthedocs.io/en/stable/kernels.html>
- HsJupyter Architecture: `docs/architecture.md`
- HsJupyter Developer Guide: `docs/developer/README.md`
