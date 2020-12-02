#!/usr/bin/env bash

ulimit -v 3000000
ghc -O2 Bug.hs

[[ $? -eq 251 ]]
