#!/usr/bin/env bash

rm -f Bug
ghc -rtsopts -O2 Bug.hs > /dev/null

rm -f output.txt
timeout 25s ./Bug 2 +RTS -t --machine-readable -RTS > output.txt 2>&1
A_BROKEN=$?
A=$(grep -oP 'bytes allocated\", \"\K[0-9]*' output.txt)

rm -f output.txt
timeout 25s ./Bug 1 +RTS -t --machine-readable -RTS > output.txt 2>&1
B_BROKEN=$?
B=$(grep -oP 'bytes allocated\", \"\K[0-9]*' output.txt)


RATIO=$(python -c "print ($A * 1.0/ $B)")
echo $RATIO

BIG_ENOUGH=$(python -c "print ($RATIO > 15.0)")
SMALL_ENOUGH=$(python -c "print ($RATIO < 64.0)")

[[ $A_BROKEN -eq 0 && $B_BROKEN -eq 0 && $BIG_ENOUGH == "True" && $SMALL_ENOUGH == "True" ]]
