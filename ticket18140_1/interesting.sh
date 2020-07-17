#!/run/current-system/sw/bin/bash

timeout 25s ghc -XFlexibleContexts Bug -O2 

if [ $? = 124 ] || [ $? = 137 ]; then
    exit 0;
else
    exit 1;
fi;
