#!/run/current-system/sw/bin/bash

ghc -O0 Bug.hs -rtsopts > /dev/null
FAST=$(./Bug +RTS -t --machine-readable -RTS 2>&1 | grep -oP 'mutator_wall_seconds\", \"\K[0-9]*\.[0-9]*')

ghc -O1 Bug.hs -rtsopts > /dev/null
SLOW=$(./Bug +RTS -t --machine-readable -RTS 2>&1 | grep -oP 'mutator_wall_seconds\", \"\K[0-9]*\.[0-9]*')

RATIO=$(python -c "print ($SLOW * 1.0/ $FAST)")
echo $RATIO

RESULT=$(python -c "print ($RATIO > 4.0)")
[[ $RESULT == "True" ]]
