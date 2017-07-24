{ supportedSystems ? ["x86_64-linux"]
, supportedCompilers ? ["ghc7103" "ghc802" "ghc821"]
}:

with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });

let
  pkgs = import <nixpkgs> {};

  configurations = 
    pkgs.lib.listToAttrs (
      pkgs.lib.concatMap (compiler: 
        pkgs.lib.concatMap (system: 
          [{name = "fp-course_" + compiler + "_" + system; value = {inherit compiler system;};}]
        ) supportedSystems
      ) supportedCompilers
    );

  jobs =
      pkgs.lib.mapAttrs (name: configuration: 
          let
            compiler = configuration.compiler; 
            system = configuration.system; 
            nixpkgs = { pkgs = pkgsFor system; };
            course = import ../default.nix { inherit nixpkgs compiler; };
          in
            course
      ) configurations;
in
  jobs
