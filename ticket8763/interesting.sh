#!/run/current-system/sw/bin/bash

ghc -O Bug.hs -rtsopts > /dev/null
A=$(./Bug +RTS -t --machine-readable -RTS 2>&1 | grep -oP 'bytes allocated\", \"\K[0-9]*')

ghc -O2 Bug.hs -rtsopts > /dev/null
B=$(./Bug +RTS -t --machine-readable -RTS 2>&1 | grep -oP 'bytes allocated\", \"\K[0-9]*')

RATIO=$(python -c "print ($A * 1.0/ $B)")
echo $RATIO

RESULT=$(python -c "print ($RATIO > 5.0)")
[[ $RESULT == "True" ]]
