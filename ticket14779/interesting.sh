#!/usr/bin/env bash

OUTPUT=output.txt
ERROR=error.txt

timeout 30s ghc -O -dcore-lint -g -c Bug.hs > $OUTPUT 2> $ERROR

grep "Compilation had errors" $ERROR &&
grep "*** Core Lint errors : in result of Simplifier ***" $OUTPUT &&
grep "The type of this binder is unlifted:" $OUTPUT
