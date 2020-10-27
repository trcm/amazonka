{ pkgs ? import <nixpkgs> {} }:

let drv = pkgs.haskellPackages.callPackage ./default.nix {};

in pkgs.mkShell {

  buildInputs = [
    drv
    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
