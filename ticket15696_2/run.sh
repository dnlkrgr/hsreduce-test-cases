#!/run/current-system/sw/bin/bash
nix-shell --run '/home/daniel/.cabal/bin/hsreduce 8 /home/daniel/workspace/hsreduce-test-cases/ticket15696_2 interesting.sh Bug.hs +RTS -qa -s'
