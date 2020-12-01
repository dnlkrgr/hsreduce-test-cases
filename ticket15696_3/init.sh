#!/usr/bin/env bash

DIR_NAME=containers

cp Main.hs $DIR_NAME
cp containers.cabal $DIR_NAME
cp shell.nix $DIR_NAME
cp ghc861.nix $DIR_NAME
cp ghc843.nix $DIR_NAME
cp hie.yaml $DIR_NAME
cp interesting.sh $DIR_NAME

cd $DIR_NAME

nix-shell --run 'hsreduce merge --sourceFile Main.hs'

echo ''
echo 'Next steps:'
echo '1. inside containers/AllInOne.hs: Rename "module AllInOne where" to "module Main where"'
echo '2. run: `nix-shell ghc861.nix --run "hsreduce reduce --numberOfThreads 8 --test interesting.sh --sourceFile AllInOne.hs --timeOut 15"`'
