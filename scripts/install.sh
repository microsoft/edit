#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

# Installs the edit binary into Cargo's bin dir.
# Additional arguments are forwarded to `cargo install`.
cargo install --path . --locked "$@"
