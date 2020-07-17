#!/run/current-system/sw/bin/bash

timeout 25s ghc -O2 Bug.hs

if [ $? = 124 ] || [ $? = 137 ]; then
    exit 0;
else
    exit 1;
fi;
