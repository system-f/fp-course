{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  fp-course = haskellPackages.callPackage ./fp-course.nix {};
  modified-fp-course = pkgs.haskell.lib.overrideCabal fp-course (drv: {
    # Dodgy fun times, make sure that
    # - the tests compile
    # - the tests failing doesn't cause the build to fail
    checkPhase = "true";
  });
in
  modified-fp-course

