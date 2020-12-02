with import (builtins.fetchGit {
  name = "nixos-unstable-arst";
  url = "https://github.com/nixos/nixpkgs-channels/";
  ref = "refs/heads/nixos-unstable";
  rev = "8d05772134f17180fb2711d0660702dae2a67313";
}) {};

let ghc = haskell.packages.ghc8101.ghcWithPackages (p: with p; [ aeson bytestring syb vector utf8-string integer-gmp base16-bytestring ]);
# let ghc = haskellPackages.ghcWithPackages (p: with p; [ aeson bytestring syb vector utf8-string integer-gmp base16-bytestring ]);
in mkShell {
  name = "ticket18140_1";
  buildInputs = [ ghc ];
}
