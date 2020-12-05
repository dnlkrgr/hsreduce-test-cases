#!/usr/bin/env bash

DIR_NAME=protocol-buffers
DIR_NAME_LOWER=$DIR_NAME/descriptor

# initializing submodule
cd $DIR_NAME
git submodule init
git submodule update
cd ..

cp interesting.sh $DIR_NAME_LOWER
cp ghc8101.nix $DIR_NAME_LOWER
cp hie.yaml $DIR_NAME_LOWER
cp shell.nix $DIR_NAME_LOWER

cd $DIR_NAME_LOWER
cabal update

echo 'merging project ...'
nix-shell --run '~/.cabal/bin/hsreduce merge --sourceFile src-auto-generated/Text/DescriptorProtos/FileOptions.hs'
cd ../
# primitive version of "inlining dependencies"
cp -r Text descriptor
cd ../$DIR_NAME_LOWER

echo ''
echo 'is merged file interesting?'
nix-shell ghc8101.nix --run './interesting.sh'

echo ''
echo 'Next steps:'
echo '1. cd protocol-buffers/descriptor'
echo '2. run `hsreduce reduce --test interesting.sh --sourceFile AllInOne.hs --numberOfThreads 2 --timeOut 160`'
