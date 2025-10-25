# HsJupyter

HsJupyter is a next-generation Jupyter kernel for the Haskell programming language. The project is currently in the design and planning phase while we build a streamlined installation experience and a modern runtime architecture.

## Features

- Persistent GHC-powered execution engine tuned for fast feedback loops
- Rich output handling (text, HTML, images) with extensible renderers
- Planned support for completions, widgets, diagnostics, and resource monitoring
- Distribution-first approach with prebuilt binaries and a one-command installer
- Architecture guided by DRY, KISS, and YAGNI principles for maintainability

## Quickstart

Prereqs:

- GHC via ghcup: `ghcup install ghc 9.12.2 cabal`
- Python (optional) with `pyzmq` for the demo client

Build fast (dev mode):

```bash
cabal build lib:hs-jupyter-kernel -O0
```

Run the prototype kernel against a sample Jupyter connection file:

```bash
cabal v2-run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Info
```

Run tests (targeted and full):

```bash
cabal test unit -O0 --test-option="--match=/GHCSession/"
cabal v2-test
```

Optional: drive an execute-echo roundtrip from Python (see `scripts/demo/README.md`).

## Documentation

- [Architecture Overview](docs/architecture.md)
- [Roadmap](docs/roadmap.md)
- [Developer Guide](docs/developer/README.md)
- [Installation (WIP)](docs/installation/README.md)
- Protocol references: <https://jupyter-client.readthedocs.io/en/stable/>

Current prototype entrypoint: `app/KernelMain.hs`.

Run the echo demo against a Jupyter connection file:

```bash
cabal v2-run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Info
```

CLI flags come from `app/KernelMain.hs`:

- `--connection FILE` – path to the Jupyter connection file
- `--log-level (Debug|Info|Warn|Error)` – overrides `HSJUPYTER_LOG_LEVEL`

## Status

- ✅ Architecture, planning, and roadmap documentation
- 🔄 Implementation in progress (kernel bridge, runtime, tooling)
- ⏳ Installer, knowledge base, and benchmarking suite

Key modules implemented:

- `HsJupyter.KernelProcess` – process lifecycle and socket management
- `HsJupyter.Bridge.*` – ZeroMQ bridge, heartbeat, protocol envelopes/codecs
- `HsJupyter.Router.RequestRouter` – request routing (scaffolded)
- `HsJupyter.Runtime.*` – runtime manager, GHC session, diagnostics, telemetry

## CLI Reference

Binary: `hs-jupyter-kernel` (entrypoint in `app/KernelMain.hs`)

Flags:

- `--connection FILE` — path to Jupyter connection JSON
- `--log-level Debug|Info|Warn|Error` — overrides env

Environment:

- `HSJUPYTER_LOG_LEVEL` — default log level when flag not provided

Exit messages are printed to stdout; structured logs and telemetry will be wired per `docs/architecture.md`.

## Demo: Python Client

Use the helper in `scripts/demo/` to exercise an `execute_request` roundtrip without a full Jupyter stack.

Prereq: `pip install pyzmq`

Steps:

- Terminal A: start the kernel

```bash
cabal v2-run hs-jupyter-kernel -- \
  --connection scripts/demo/sample-connection.json \
  --log-level Debug
```

- Terminal B: send an execute request

```bash
python3 scripts/demo/phase1_echo_notebook.py \
  --connection scripts/demo/sample-connection.json
```

The script prints reply frames and any iopub stream/output captured by the echo runtime.

Excerpt (simplified) from `scripts/demo/phase1_echo_notebook.py` showing how the execute_request is built and signed:

```python
def build_execute_request(code: str, key: bytes) -> List[bytes]:
    header = {
        "msg_id": uuid.uuid4().hex,
        "session": uuid.uuid4().hex,
        "username": "demo",
        "msg_type": "execute_request",
        "version": "5.3",
        "date": time.strftime("%Y-%m-%dT%H:%M:%S"),
    }
    content = {"code": code, "silent": False, "store_history": True, "allow_stdin": False}
    encoded = [canonical_json(header), canonical_json({}), canonical_json({}), canonical_json(content)]
    signature = sign_frames(key, encoded)
    return [b"", signature.encode("utf-8"), *encoded]
```

This mirrors the Jupyter wire format used by the bridge (`Envelope` and `Codec` in `src/HsJupyter/Bridge/Protocol/`).

## Connection File Reference

The kernel reads a Jupyter connection JSON and binds ZeroMQ sockets accordingly. Minimal fields used by `app/KernelMain.hs` and `HsJupyter.KernelProcess`:

- `ip`: host to bind (e.g., "127.0.0.1")
- `transport`: scheme (e.g., "tcp")
- `shell_port`, `iopub_port`, `control_port`, `stdin_port`, `hb_port`: port numbers
- `signature_scheme`: "hmac-sha256" or empty for none
- `key`: HMAC key (string)

See a working sample in `scripts/demo/sample-connection.json`.

## Troubleshooting

- Ports already in use
  - Edit `scripts/demo/sample-connection.json` to change `shell_port`, `iopub_port`, etc., then pass it via `--connection`.
- Missing GHC/cabal
  - Install via ghcup: `ghcup install ghc 9.12.2 cabal` and ensure they’re on PATH.
- pyzmq not installed
  - `pip install --user pyzmq` or use a virtualenv.
- ZeroMQ library issues
  - Ensure libzmq is available on your system. On Debian/Ubuntu: `sudo apt-get install libzmq3-dev`.

## Contributing

While the kernel is still under active design, we welcome feedback and discussion on the architecture. Please open an issue to share ideas or ask questions about the roadmap.
