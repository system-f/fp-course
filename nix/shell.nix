(import ./default.nix).shellFor {
  tools = {
    cabal = "3.2.0.0";
    haskell-language-server = "latest";
  };
}
