#!/usr/bin/env bash

timeout 37s ghc -O2 -XFlexibleContexts Bug.hs 

[[ $? -eq 124 || $? -eq 137 ]]
