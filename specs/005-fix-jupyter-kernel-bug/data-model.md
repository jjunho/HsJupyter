# Data Model: Jupyter Kernel Integration Bug Fix

This feature is a bug fix and does not introduce any new user-facing data models. The primary data structures involved are the internal representations of the Jupyter messaging protocol, which are already defined within the project.

## Key Entities (Internal)

-   **Message**: Represents a single message in the Jupyter protocol. It consists of a header, parent header, metadata, and content.
-   **ConnectionInfo**: Contains the information from the `connection.json` file that Jupyter provides to the kernel on startup. This includes IP addresses, ports, and the session key for message signing.
-   **KernelState**: An in-memory data structure that holds the state of the kernel, including the GHC session for code evaluation and any user-defined variables.

These entities are internal to the kernel's implementation and will not be exposed to the user or persisted.
