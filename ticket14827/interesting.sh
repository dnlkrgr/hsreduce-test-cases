#!/usr/bin/env bash

ghc -O0 Bug.hs -rtsopts > /dev/null
FAST=$(timeout 25s ./Bug +RTS -t --machine-readable -RTS 2>&1 | grep -oP 'mutator_wall_seconds\", \"\K[0-9]*\.[0-9]*')

ghc -O1 Bug.hs -rtsopts > /dev/null
SLOW=$(timeout 25s ./Bug +RTS -t --machine-readable -RTS 2>&1 | grep -oP 'mutator_wall_seconds\", \"\K[0-9]*\.[0-9]*')

RATIO=$(python -c "print ($SLOW * 1.0/ $FAST)")
echo $RATIO

BIG_ENOUGH=$(python -c "print ($RATIO > 4.0)")
SMALL_ENOUGH=$(python -c "print ($RATIO < 32.0)")
[[ $BIG_ENOUGH == "True" && $SMALL_ENOUGH == "True" ]]
