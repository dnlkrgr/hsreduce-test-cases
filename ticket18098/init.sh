#!/usr/bin/env bash

cabal update

cd ghc.nix
git submodule init
git submodule update
cd ..

cd ghc
git submodule init
git submodule update

nix-shell ../ghc.nix --run "./boot && configure_ghc && hadrian/build -j --flavour=quick"
