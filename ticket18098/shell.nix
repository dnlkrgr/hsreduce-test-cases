with import <nixpkgs> {};
pkgs.mkShell {
  name = "no-inputs-needed";
  buildInputs = [];
}
