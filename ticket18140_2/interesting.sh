#!/usr/bin/env bash

ulimit -v 3000000
timeout 140s ghc -O2 -XFlexibleContexts Bug.hs 

[[ $? -eq 251 ]]
