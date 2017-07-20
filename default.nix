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

  fp-course = modifiedHaskellPackages.callPackage ./fp-course.nix {};
  # Dodgy fun times, make sure that 
  # - the tests compile
  # - the tests failing doesn't cause the build to fail
  modified-fp-course = pkgs.haskell.lib.overrideCabal fp-course (drv: { checkPhase = "true"; });
in
  modified-fp-course

