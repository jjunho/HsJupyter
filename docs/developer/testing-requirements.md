# Testing Requirements

## Runtime Statistics (RTS)

Several tests in the project require GHC's Runtime Statistics to be enabled to monitor memory usage and performance metrics.

### Running Tests with RTS Stats

To run tests that use performance monitoring and memory statistics:

```bash
cabal test unit -O0 --test-options="+RTS -T -RTS"
cabal test integration -O0 --test-options="+RTS -T -RTS"
```

### Tests Requiring RTS Stats

The following test categories require RTS statistics:

- **GHCRuntime timeout behavior tests**: Monitor execution time and memory for differentiated timeouts
- **Performance monitoring tests**: Track execution time, memory usage, and error telemetry  
- **Memory limit tests**: Enforce and monitor memory constraints
- **Resource guard tests**: Memory monitoring and limit enforcement

### Without RTS Stats

Running tests without `-T` will result in failures with:

```text
IOException of type UnsupportedOperation
unsupported operation (GHC.Stats.getRTSStats: GC stats not enabled. Use `+RTS -T -RTS' to enable them.)
```

### Production Configuration

In production kernels, enable RTS stats for monitoring:

```bash
./hs-jupyter-kernel +RTS -T -RTS
```

This enables the performance telemetry and resource monitoring features implemented in Phase 7.
