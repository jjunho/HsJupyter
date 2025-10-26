# HsJupyter CLI Troubleshooting Guide

**Version**: 0.1.0.0
**Last Updated**: 2025-01-28

## Overview

This guide provides solutions for common issues encountered when using the HsJupyter CLI. Most problems can be diagnosed using the `doctor` command and resolved with the steps outlined below.

## Quick Diagnosis

Always start troubleshooting with the diagnostic command:

```bash
# Basic system check
hs-jupyter-kernel doctor

# Detailed diagnostics with report
hs-jupyter-kernel doctor --verbose --report diagnostic-report.json

# Check specific components
hs-jupyter-kernel doctor --check jupyter
hs-jupyter-kernel doctor --check kernel
hs-jupyter-kernel doctor --check ghc
```

## Common Issues and Solutions

### Installation Issues

#### "Permission denied" during installation

**Symptoms:**

- Exit code 5
- Error message: "Permission denied when creating kernelspec directory"

**Solutions:**

1. **User installation (recommended):**

   ```bash
   hs-jupyter-kernel install --user
   ```

2. **System installation (requires admin privileges):**

   ```bash
   sudo hs-jupyter-kernel install --system
   ```

3. **Custom directory:**

   ```bash
   hs-jupyter-kernel install --kernelspec-dir ~/.local/share/jupyter/kernels
   ```

#### "Jupyter not found" error

**Symptoms:**

- Exit code 10
- Error message: "Jupyter environment not found"

**Solutions:**

1. **Install Jupyter:**

   ```bash
   # Using pip
   pip install jupyter

   # Using conda
   conda install jupyter

   # Using system package manager
   sudo apt install jupyter  # Ubuntu/Debian
   sudo dnf install jupyter  # Fedora/RHEL
   ```

2. **Verify Jupyter installation:**

   ```bash
   jupyter --version
   jupyter kernelspec list
   ```

3. **Custom Jupyter path:**

   ```bash
   export PATH="/path/to/jupyter/bin:$PATH"
   hs-jupyter-kernel install
   ```

#### "GHC not found" error

**Symptoms:**

- Exit code 11
- Error message: "GHC compiler not found"

**Solutions:**

1. **Install GHC via ghcup (recommended):**

   ```bash
   # Install ghcup
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

   # Install latest GHC
   ghcup install ghc latest
   ghcup set ghc latest
   ```

2. **Install system GHC:**

   ```bash
   # Ubuntu/Debian
   sudo apt install ghc

   # Fedora/RHEL
   sudo dnf install ghc

   # macOS with Homebrew
   brew install ghc
   ```

3. **Specify custom GHC path:**

   ```bash
   hs-jupyter-kernel install --ghc-path /usr/local/bin/ghc
   ```

4. **Set environment variable:**

   ```bash
   export HSJUPYTER_GHC_PATH=/path/to/ghc
   hs-jupyter-kernel install
   ```

#### Installation succeeds but kernel not available in Jupyter

**Symptoms:**

- Installation reports success
- Kernel not listed in Jupyter notebook/lab

**Solutions:**

1. **Check kernel registration:**

   ```bash
   hs-jupyter-kernel list
   jupyter kernelspec list
   ```

2. **Reinstall kernel:**

   ```bash
   hs-jupyter-kernel install --force
   ```

3. **Restart Jupyter:**

   ```bash
   # Stop any running Jupyter processes
   pkill -f jupyter

   # Restart Jupyter
   jupyter notebook
   ```

4. **Check kernel.json file:**

   ```bash
   cat ~/.local/share/jupyter/kernels/haskell/kernel.json
   ```

### Runtime Issues

#### Kernel fails to start in Jupyter

**Symptoms:**

- Kernel selection shows "Haskell" but fails to start
- Error messages in Jupyter console

**Solutions:**

1. **Check kernel functionality:**

   ```bash
   hs-jupyter-kernel doctor --check kernel
   ```

2. **Test kernel manually:**

   ```bash
   # Find kernel executable
   hs-jupyter-kernel list

   # Test kernel process (replace with actual path)
   /path/to/hs-jupyter-kernel --kernel
   ```

3. **Check kernel.json configuration:**

   ```json
   {
     "argv": ["/path/to/hs-jupyter-kernel", "--kernel"],
     "display_name": "Haskell",
     "language": "haskell"
   }
   ```

4. **Verify executable permissions:**

   ```bash
   ls -la /path/to/hs-jupyter-kernel
   chmod +x /path/to/hs-jupyter-kernel
   ```

#### "Connection failed" errors

**Symptoms:**

- Kernel starts but loses connection
- Timeout errors in Jupyter

**Solutions:**

1. **Check network/firewall settings:**

   ```bash
   # Test ZeroMQ connectivity
   python3 -c "import zmq; print('ZeroMQ OK')"
   ```

2. **Verify port availability:**

   ```bash
   netstat -tlnp | grep :8888
   ```

3. **Increase timeout settings:**

   ```bash
   # In Jupyter configuration
   c.KernelManager.kernel_info_timeout = 60
   c.KernelManager.shutdown_wait_time = 10
   ```

#### Memory or performance issues

**Symptoms:**

- Kernel crashes with out-of-memory errors
- Slow execution or hangs

**Solutions:**

1. **Check system resources:**

   ```bash
   free -h  # Linux
   vm_stat   # macOS
   ```

2. **Monitor kernel resource usage:**

   ```bash
   # Enable resource monitoring
   hs-jupyter-kernel install --memory-limit 1024 --exec-timeout 300
   ```

3. **Optimize Haskell compilation:**

   ```bash
   # Use optimization flags
   export HSJUPYTER_OPTIMIZATION="-O1"
   ```

### Cross-Platform Issues

#### Windows-specific issues

**Symptoms:**

- Path separator issues
- Permission problems
- Unicode encoding errors

**Solutions:**

1. **Use Windows paths correctly:**

   ```cmd
   hs-jupyter-kernel install --kernelspec-dir "C:\Users\%USERNAME%\AppData\Roaming\jupyter\kernels"
   ```

2. **Install in WSL if available:**

   ```bash
   # From WSL terminal
   hs-jupyter-kernel install --user
   ```

3. **Check Windows Defender exclusions:**
   - Add Haskell and Jupyter directories to Windows Defender exclusions

#### macOS-specific issues

**Symptoms:**

- Gatekeeper blocks execution
- Path issues with Homebrew

**Solutions:**

1. **Allow execution:**

   ```bash
   # Remove quarantine attribute
   xattr -rd com.apple.quarantine /path/to/hs-jupyter-kernel
   ```

2. **Use Homebrew paths:**

   ```bash
   export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"
   hs-jupyter-kernel install
   ```

### Diagnostic and Logging

#### Enable verbose logging

```bash
# Environment variable
export HSJUPYTER_LOG_LEVEL=debug

# Command line
hs-jupyter-kernel install --verbose

# Save logs
hs-jupyter-kernel doctor --report debug-report.json
```

#### Collect system information

```bash
# Full system diagnostic
hs-jupyter-kernel doctor --verbose --json > full-diagnostic.json

# Include system info
uname -a >> diagnostic-info.txt
ghc --version >> diagnostic-info.txt
jupyter --version >> diagnostic-info.txt
```

### Advanced Troubleshooting

#### Manual kernel registration

If automatic installation fails:

```bash
# Create kernel directory
mkdir -p ~/.local/share/jupyter/kernels/haskell

# Create kernel.json
cat > ~/.local/share/jupyter/kernels/haskell/kernel.json << EOF
{
  "argv": ["/path/to/hs-jupyter-kernel", "--kernel"],
  "display_name": "Haskell",
  "language": "haskell",
  "metadata": {
    "debugger": false
  }
}
EOF
```

#### Test kernel communication

```python
# test_kernel.py
import jupyter_client
import json

# Test kernel manager
km = jupyter_client.KernelManager(kernel_name='haskell')
km.start_kernel()

# Test execution
kc = km.client()
msg_id = kc.execute('putStrLn "Hello World"')
reply = kc.get_shell_msg(timeout=10)

print("Kernel response:", json.dumps(reply, indent=2))
km.shutdown_kernel()
```

#### Debug ZeroMQ connections

```bash
# Check ZeroMQ installation
python3 -c "import zmq; print(zmq.zmq_version())"

# Test socket binding
python3 -c "
import zmq
ctx = zmq.Context()
socket = ctx.socket(zmq.REP)
socket.bind('tcp://127.0.0.1:5555')
print('Socket bound successfully')
socket.close()
"
```

## Getting Help

### Report Issues

When reporting issues, please include:

1. **Diagnostic report:**

   ```bash
   hs-jupyter-kernel doctor --verbose --report issue-report.json
   ```

2. **System information:**

   ```bash
   uname -a
   ghc --version
   jupyter --version
   python3 --version
   ```

3. **Installation logs:**

   ```bash
   hs-jupyter-kernel install --verbose 2>&1 | tee install.log
   ```

4. **Error messages and stack traces**

### Community Support

- **GitHub Issues:** Report bugs and request features
- **Documentation:** Check the [CLI Usage Guide](usage.md)
- **Development:** See [Developer Guide](../developer/README.md)

## Exit Code Reference

| Code | Meaning | Common Causes |
|------|---------|---------------|
| 0 | Success | - |
| 1 | General error | Unexpected failures |
| 2 | Invalid arguments | Wrong command syntax |
| 3 | Installation failed | Permission issues, missing dependencies |
| 4 | Validation failed | Configuration errors |
| 5 | Permission denied | Access restrictions |
| 10 | Jupyter not found | Missing Jupyter installation |
| 11 | GHC not found | Missing Haskell compiler |
| 12 | Kernel not functional | Runtime errors, missing libraries |

## Prevention

### Best Practices

1. **Regular updates:**

   ```bash
   # Keep tools updated
   ghcup upgrade
   pip install --upgrade jupyter
   ```

2. **Test installations:**

   ```bash
   # Validate after installation
   hs-jupyter-kernel doctor
   ```

3. **Backup configurations:**

   ```bash
   # Backup kernel configurations
   cp -r ~/.local/share/jupyter/kernels/haskell ~/haskell-kernel-backup
   ```

4. **Monitor resources:**

   ```bash
   # Regular health checks
   hs-jupyter-kernel doctor --quiet
   ```

### Environment Setup

Create a robust development environment:

```bash
# .bashrc or .zshrc additions
export PATH="$HOME/.ghcup/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

# HsJupyter specific
export HSJUPYTER_LOG_LEVEL=info
export HSJUPYTER_GHC_PATH="$(which ghc)"

# Verify setup
hs-jupyter-kernel version --check-compatibility
```
