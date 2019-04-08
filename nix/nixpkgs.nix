import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2019-04-08";
  url = https://github.com/nixos/nixpkgs/;
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  rev = "acbdaa569f4ee387386ebe1b9e60b9f95b4ab21b";
}) {}
