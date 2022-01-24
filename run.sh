#!/usr/bin/env bash
set -euo pipefail

cabal run -O2 -v0 . -- "$@"
