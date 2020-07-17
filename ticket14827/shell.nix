with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/18.03.tar.gz) {};
let 
    ghc = haskell.packages.ghc822.ghcWithPackages (p: [ p.text ]);
in mkShell { 
    name        = "ghc822AndText"; 
    buildInputs = [
        ghc
    ];
}
