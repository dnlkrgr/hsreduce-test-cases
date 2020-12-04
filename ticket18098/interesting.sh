#!/usr/bin/env bash
timeout 10s ghc/_build/stage1/bin/ghc -O2 -dcore-lint -fforce-recomp Bug.hs > output.txt 2> error.txt
grep "*** Core Lint errors : in result of Simplifier ***" output.txt
grep "The type of this binder doesn't match the type of its RHS:" output.txt
grep "Compilation had errors" error.txt
