#!/usr/bin/env bash
# Thin wrapper for Unix-like systems; the logic lives in reproduce.R.
set -euo pipefail
cd "$(dirname "$0")"
Rscript reproduce.R "$@"
