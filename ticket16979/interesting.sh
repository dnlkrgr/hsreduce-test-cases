#!/usr/bin/env bash
ERROR=error.txt
OUTPUT=output.txt
ghc -O -dcore-lint -g -c Bug.hs > $OUTPUT 2> $ERROR

grep "Compilation had errors" $ERROR &&
grep "*** Core Lint errors : in result of Simplifier ***" $OUTPUT &&
grep "no location info>: warning:" $OUTPUT && 
grep "In the expression: " $OUTPUT
