#!/usr/bin/env bash

ulimit -v 3000000
timeout 120s ghc -O2 Bug.hs

[[ $? -eq 251 ]]
