#!/run/current-system/sw/bin/bash
/home/daniel/head.hackage/result/bin/ghc -O2 -dcore-lint -fforce-recomp Bug.hs > output.txt 2> error.txt
grep "*** Core Lint errors : in result of Simplifier ***" output.txt
grep "Compilation had errors" error.txt
