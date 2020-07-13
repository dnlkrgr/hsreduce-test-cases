with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz) {};
let ghc = pkgs.haskell.compiler.ghc865;
in mkShell {
  name = "ghc865";
  buildInputs = [
    ghc
  ];
}
