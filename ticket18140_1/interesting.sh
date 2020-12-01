#!/usr/bin/env bash

ulimit -v 6000000
ghc -O2 -XFlexibleContexts Bug.hs 

[[ $? -eq 251 ]]
