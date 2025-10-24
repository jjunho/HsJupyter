# Phase 1 Quickstart: Execute Echo Demo

## Prerequisites

- GHC 9.6.4 and cabal installed via `ghcup`.
- Python 3.11 with `pyzmq` installed (`python -m pip install pyzmq`).
- ZeroMQ library (`libzmq`) present on the system.

## Build Kernel Prototype

```bash
cabal v2-build hs-jupyter-kernel
```

## Launch Demo Harness

1. Start the kernel with the sample connection file (edit ports if needed):

   ```bash
   cabal v2-run hs-jupyter-kernel -- --connection scripts/demo/sample-connection.json --log-level debug
   ```

2. In another terminal, run the helper to send the execute request via pyzmq:

   ```bash
   scripts/demo/phase1_echo_notebook.py --connection scripts/demo/sample-connection.json
   ```

## Expected Results

- Demo helper sends a signed `execute_request` and prints the resulting `execute_reply` and stream frames.
- Run `cabal v2-test` to execute unit/integration suites (not run in this environment).
- Kernel logs show bound endpoints, message IDs, and heartbeat latency every 5 seconds.
- Introducing an invalid signature in the demo script (for example by editing the connection key) triggers a rejection log while sockets remain available.

## Troubleshooting

- If sockets fail to bind, verify the ports listed in the connection file are free.
- Missing ZeroMQ errors indicate `libzmq` headers or runtime are absent; reinstall via package manager (`brew install zeromq`, `apt install libzmq3-dev`).
- To inspect raw traffic, run the kernel with `HSJUPYTER_LOG_LEVEL=trace` to see frame-level dumps.
