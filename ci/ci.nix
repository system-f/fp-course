let
  pkgs = import <nixpkgs> {};

  fp-course = import ../default.nix {};

  jobs = rec {
    inherit fp-course;
  };
in
  jobs
