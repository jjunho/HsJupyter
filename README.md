# HsJupyter

HsJupyter is a next-generation Jupyter kernel for the Haskell programming language. The project is currently in the design and planning phase while we build a streamlined installation experience and a modern runtime architecture.

## Features

- Persistent GHC-powered execution engine tuned for fast feedback loops
- Rich output handling (text, HTML, images) with extensible renderers
- Planned support for completions, widgets, diagnostics, and resource monitoring
- Distribution-first approach with prebuilt binaries and a one-command installer
- Architecture guided by DRY, KISS, and YAGNI principles for maintainability

## Documentation

- [Architecture Overview](docs/architecture.md)
- Jupyter protocol references: <https://jupyter-client.readthedocs.io/en/stable/>

Additional guides (installation, user handbook, API reference) will land as implementation milestones are completed.

## Status

- ✅ Architecture, planning, and roadmap documentation
- 🔄 Implementation in progress (kernel bridge, runtime, tooling)
- ⏳ Installer, knowledge base, and benchmarking suite

## Contributing

While the kernel is still under active design, we welcome feedback and discussion on the architecture. Please open an issue to share ideas or ask questions about the roadmap.
