#!/usr/bin/env bash

timeout 30s ghc -O2 -XFlexibleContexts Bug.hs 

[[ $? -eq 124 || $? -eq 137 ]]
