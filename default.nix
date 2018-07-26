{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    tasty = pkgs.fetchFromGitHub {
      owner = "feuerbach";
      repo = "tasty";
      rev = "core-1.1.0.1";
      sha256 = "03fcc75l5mrn5dwh6xix5ggn0qkp8kj7gzamb6n2m42ir6j7x60l";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      tasty = super.callCabal2nix "tasty" "${sources.tasty}/core" {};
      tasty-hunit = super.callCabal2nix "tasty" "${sources.tasty}/hunit" {};
      tasty-quickcheck = super.callCabal2nix "tasty" "${sources.tasty}/quickcheck" {};
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

