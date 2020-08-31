with import <nixpkgs> {};
let 
    pinnedNixPkgs = import (builtins.fetchGit {
      name = "nixos-unstable-2018-09-12";
      url = "https://github.com/nixos/nixpkgs-channels/";
      ref = "refs/heads/nixos-16.09";
      rev = "25f4906da6387e132823417bc54ea86040fb9bd5";
    }) {};
    ghc = pinnedNixPkgs.haskell.packages.ghc801.ghcWithPackages (p: [ p.vector ]);
in mkShell { 
    name        = "ghc801AndVector"; 
    buildInputs = [
        python
        ghc
    ];
}
