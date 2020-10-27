{ pkgs ? import <nixpkgs> {}}:

let
  amazonka-core = pkgs.haskellPackages.callPackage ../core/default.nix {};
  amazonka-test = pkgs.haskellPackages.callPackage ../test/default.nix {
    amazonka-core=amazonka-core;
  };
  amazonka-sts = pkgs.haskellPackages.callPackage ../amazonka-sts/default.nix {
    amazonka-core=amazonka-core;
    amazonka-test=amazonka-test;
  };
  drv = pkgs.haskell.lib.addBuildDepends
    (pkgs.haskellPackages.callPackage ./default.nix {
      amazonka-core = amazonka-core;
      amazonka-sts = amazonka-sts;
    })
    (with pkgs.haskellPackages; [ ghcid ]);
in drv.env
