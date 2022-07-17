#!/bin/sh
cabal run -O2 -v0 . -- "$@"
