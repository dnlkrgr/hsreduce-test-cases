with import <nixpkgs> {};
let 
    ghc = haskell.packages.ghc881.ghcWithPackages (p: [ p.vector ]);
in mkShell { 
    name        = "ghc881AndVector"; 
    buildInputs = [
        ghc
    ];
}
