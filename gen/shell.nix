{pkgs ? import <nixpkgs> {}}:

let
# 39cd40f7bea40116ecb756d46a687bfd0d2e550e

  nixpkgs-source = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "3e1be2206b4c1eb3299fb633b8ce9f5ac1c32898";
    sha256 = "11d01fdb4d1avi7564h7w332h3jjjadsqwzfzpyh5z56s6rfksxc";
  };

  nixpkgs = import nixpkgs-source {};
  ghc = nixpkgs.haskell.compiler.ghc822;
  ghc822 = nixpkgs.haskell.compiler.ghc822;
  drv =
    nixpkgs.haskell.lib.addBuildDepend
      (nixpkgs.haskellPackages.callPackage ./default.nix {})
      ((with nixpkgs.haskellPackages;
        [ stack ghcid ghc ghc822 stylish-haskell
        ]));
in drv.env
