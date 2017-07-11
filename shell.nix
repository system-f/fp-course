{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      tasty-discover = pkgs.haskell.lib.dontCheck super.tasty-discover_3_0_2;
    };
  };

  drv = modifiedHaskellPackages.callPackage ./fp-course.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
