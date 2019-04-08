import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-18.09-2019-04-08";
  url = https://github.com/nixos/nixpkgs;
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-18.09`
  rev = "222950952f15f6b1e9f036b80440b597f23e652d";
}) {}
