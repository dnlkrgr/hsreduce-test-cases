#!/run/current-system/sw/bin/bash
ghc -O2 -fforce-recomp Bug.hs && ./Bug |& grep 'Bin T2 Tip (Bin T2 Tip Tip)'