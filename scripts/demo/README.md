# Phase 1 Demo Assets

This directory contains helper scripts for exercising the Phase 1 protocol bridge without a full Jupyter stack.

- `phase1_echo_notebook.py`: sends a signed `execute_request` via pyzmq and prints the resulting reply/stream frames using the sample connection file.
- `sample-connection.json`: minimal connection file compatible with the prototype entry point (`app/KernelMain.hs`). Update the ports if the defaults clash with local services.

## Usage

```bash
# Build the prototype kernel
cabal v2-build hs-jupyter-kernel

# Run the kernel with the sample connection file and verbose logging
cabal v2-run hs-jupyter-kernel -- --connection scripts/demo/sample-connection.json --log-level Debug

# In another terminal, run the demo to send an execute_request via pyzmq
scripts/demo/phase1_echo_notebook.py --connection scripts/demo/sample-connection.json
```

The Python helper depends on `pyzmq` and exercises the real ROUTER/PUB/REP sockets exposed by the Phase 1 prototype.
