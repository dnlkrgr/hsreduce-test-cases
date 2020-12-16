#!/usr/bin/env bash

ulimit -v 6000000
timeout 150s ghc -fforce-recomp -O2 Bug.hs

[[ $? -eq 251 ]]
