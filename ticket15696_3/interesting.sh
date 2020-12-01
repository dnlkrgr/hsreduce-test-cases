#!/usr/bin/env bash
ghc -O2 AllInOne.hs && ./AllInOne |& grep "fromList \[T2_Main,T2_Main\]"
