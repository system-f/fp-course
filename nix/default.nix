let
  sources = import ./sources.nix {};
  haskellNix = import sources.haskellNix {};
  pkgs = import
    haskellNix.sources.nixpkgs-unstable
    haskellNix.nixpkgsArgs;
  # 'cleanGit' cleans a source directory based on the files known by git
  cleanSrc = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "fp-course";
    src = ./..;
  };
in(pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
    name = "fp-course";
    src = ./..;
    subDir = "support";
    # filter = path: type: path == toString ./fp-course.cabal || cleanSrc.filter path type;
  };
  compiler-nix-name = "ghc8104";
}
