{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      # work-around for some older versions of nixpkgs
      tasty-discover = 
         if super ? tasty-discover_3_0_2
         then pkgs.haskell.lib.dontCheck super.tasty-discover_3_0_2
         else  super.tasty-discover;
    };
  };

  fp-course = modifiedHaskellPackages.callPackage ./fp-course.nix {};
  modified-fp-course = pkgs.haskell.lib.overrideCabal fp-course (drv: { 
    # Dodgy fun times, make sure that 
    # - the tests compile
    # - the tests failing doesn't cause the build to fail
    checkPhase = "true"; 
  });
in
  modified-fp-course

