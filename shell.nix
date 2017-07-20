{ nixpkgs ? import <nixpkgs> {}}:
let
  inherit (nixpkgs) pkgs;
  fp-course = import ./default.nix {};
in
  if pkgs.lib.inNixShell then fp-course.env else fp-course
