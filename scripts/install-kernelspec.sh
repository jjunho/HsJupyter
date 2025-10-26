#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./scripts/install-kernelspec.sh [--kernel-bin /abs/path/to/KernelMain] [--user|--sys-prefix]
# If --kernel-bin is not provided, this script will try a few strategies to locate the built kernel executable:
#  - `cabal v2-exec -- which KernelMain`
#  - `stack exec -- which KernelMain`
#  - search dist-newstyle for an executable named KernelMain

KERNEL_BIN=""
INSTALL_SCOPE="--user"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --kernel-bin)
      KERNEL_BIN="$2"
      shift 2
      ;;
    --sys-prefix)
      INSTALL_SCOPE="--sys-prefix"
      shift
      ;;
    --user)
      INSTALL_SCOPE="--user"
      shift
      ;;
    -h|--help)
      echo "Usage: $0 [--kernel-bin PATH] [--user|--sys-prefix]"
      exit 0
      ;;
    *)
      echo "Unknown arg: $1"
      exit 1
      ;;
  esac
done

# Try to locate kernel binary if not provided
if [[ -z "$KERNEL_BIN" ]]; then
  echo "No --kernel-bin provided: trying cabal/stack/dist-newstyle heuristics..."
  # Prefer hs-jupyter-kernel binary (normal build name), fall back to KernelMain
  if command -v cabal >/dev/null 2>&1; then
    if cabal v2-exec -- which hs-jupyter-kernel >/dev/null 2>&1; then
      KERNEL_BIN=$(cabal v2-exec -- which hs-jupyter-kernel)
    elif cabal v2-exec -- which KernelMain >/dev/null 2>&1; then
      KERNEL_BIN=$(cabal v2-exec -- which KernelMain)
    fi
  fi

  if [[ -z "$KERNEL_BIN" && $(command -v stack || true) ]]; then
    if stack exec -- which hs-jupyter-kernel >/dev/null 2>&1; then
      KERNEL_BIN=$(stack exec -- which hs-jupyter-kernel)
    elif stack exec -- which KernelMain >/dev/null 2>&1; then
      KERNEL_BIN=$(stack exec -- which KernelMain)
    fi
  fi

  if [[ -z "$KERNEL_BIN" ]]; then
    # search dist-newstyle for common executable names
    KERNEL_BIN=$(find dist-newstyle -type f -perm /a+x \( -name hs-jupyter-kernel -o -name KernelMain -o -name 'hs-jupyter*' \) -print -quit 2>/dev/null || true)
  fi

  if [[ -z "$KERNEL_BIN" ]]; then
    echo "Could not locate KernelMain automatically. Build the project and retry, or pass --kernel-bin /abs/path/to/KernelMain"
    exit 2
  fi
fi

# Make sure the path is absolute
KERNEL_BIN_ABS=$(realpath "$KERNEL_BIN")
if [[ ! -x "$KERNEL_BIN_ABS" ]]; then
  echo "Found kernel binary but it is not executable: $KERNEL_BIN_ABS"
  echo "Try: chmod +x $KERNEL_BIN_ABS"
  exit 3
fi

WORKDIR=$(mktemp -d)
trap 'rm -rf "$WORKDIR"' EXIT

# Render template
TEMPLATE_FILE="$(pwd)/kernelspec/hsjupyter/kernel.json.template"
if [[ ! -f "$TEMPLATE_FILE" ]]; then
  echo "Missing template at $TEMPLATE_FILE"
  exit 4
fi

KERNEL_JSON="$WORKDIR/kernel.json"
sed "s|{{KERNEL_BIN}}|$KERNEL_BIN_ABS|g" "$TEMPLATE_FILE" > "$KERNEL_JSON"

# Copy any logos if present
if [[ -f kernelspec/hsjupyter/logo-64x64.png ]]; then
  cp kernelspec/hsjupyter/logo-64x64.png "$WORKDIR/"
fi
if [[ -f kernelspec/hsjupyter/logo-32x32.png ]]; then
  cp kernelspec/hsjupyter/logo-32x32.png "$WORKDIR/"
fi

# Install kernelspec
echo "Installing kernelspec (scope: $INSTALL_SCOPE) using kernel binary: $KERNEL_BIN_ABS"
jupyter kernelspec install "$WORKDIR" $INSTALL_SCOPE --replace

echo "Installed kernelspec. Run: jupyter kernelspec list to confirm."

echo "To uninstall later: jupyter kernelspec remove hsjupyter"

# Show installed kernel.json
KS_DIR=$(jupyter kernelspec list --json | python3 -c 'import sys, json; d=json.load(sys.stdin); print(d.get("kernelspecs", {}).get("hsjupyter", {}).get("resource_dir", ""))')
if [[ -n "$KS_DIR" ]]; then
  echo "Installed kernelspec path: $KS_DIR"
  echo "Installed kernel.json:"
  cat "$KS_DIR/kernel.json" || true
fi

exit 0
