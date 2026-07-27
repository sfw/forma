#!/usr/bin/env bash
# Compatibility wrapper for the registry-driven coverage audit.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
exec python3 "$repo_root/scripts/builtin_coverage.py" "$@"
