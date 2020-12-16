#!/usr/bin/env bash

ulimit -v 6000000
timeout 150s ghc -fforce-recomp -O2 -XFlexibleContexts Bug.hs 

[[ $? -eq 251 ]]
