{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;
in
pkgs.mkShell {
  buildInputs = with pkgs.haskellPackages; [ ghc ghcid ];
}
