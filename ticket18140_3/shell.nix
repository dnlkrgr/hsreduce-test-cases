{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  # inherit (nixpkgs) pkgs;
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2020-07-29";
    url = "https://github.com/nixos/nixpkgs-channels/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    ref = "refs/heads/nixos-unstable";
    rev = "28fce082c8ca1a8fb3dfac5c938829e51fb314c8";
  }) {};

  f = { mkDerivation, aeson, array, base, base16-bytestring, binary
      , bytestring, containers, directory, filepath, ghc-prim, mtl
      , parsec, stdenv, syb, text, utf8-string, vector
      }:
      mkDerivation {
        pname = "protocol-buffers-descriptor";
        version = "2.4.13";
        src = ./.;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          aeson array base base16-bytestring binary bytestring containers
          directory filepath ghc-prim mtl parsec syb text
          utf8-string vector
        ];
        homepage = "https://github.com/k-bx/protocol-buffers";
        description = "Text.DescriptorProto.Options and code generated from the Google Protocol Buffer specification";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
