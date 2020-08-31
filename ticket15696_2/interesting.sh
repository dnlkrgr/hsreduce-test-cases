#!/usr/bin/env bash
ghc -O2 Bug.hs && timeout 15s ./Bug |& timeout 10s grep "fromList \[T2_Main,T2_Main\]"
