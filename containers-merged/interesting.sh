#!/run/current-system/sw/bin/bash
ghc -O2 Bug.hs && ./Bug |& grep "fromList \[T2_Main,T2_Main\]"
