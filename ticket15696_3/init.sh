#!/usr/bin/env bash

DIR_NAME=containers


# intializing submodule
cd $DIR_NAME
git submodule init
git submodule update
cd ..

# copying buggy stuff
cp Main.hs $DIR_NAME
cp containers.cabal $DIR_NAME
cp shell.nix $DIR_NAME
cp ghc861.nix $DIR_NAME
cp hie.yaml $DIR_NAME
cp interesting.sh $DIR_NAME

cd $DIR_NAME

# merging project into one file
nix-shell --run '~/.cabal/bin/hsreduce merge --sourceFile Main.hs'

echo ''
echo 'is merged file interesting?'
nix-shell ghc861.nix --run './interesting.sh && echo "merged file is interesting" || echo "merged file is uninteresting"'

echo ''
echo 'Next steps:'
echo '2. cd containers'
echo '2. run: `nix-shell ghc861.nix --run "hsreduce reduce --numberOfThreads 8 --test interesting.sh --sourceFile AllInOne.hs --timeOut 15"`'
