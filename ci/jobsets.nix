{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "fp-course": {
            "enabled": 1,
            "hidden": false,
            "description": "FP Course",
            "nixexprinput": "fp-course",
            "nixexprpath": "./ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "fp-course": { "type": "git", "value": "https://github.com/data61/fp-course.git master", "emailresponsible": false },
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git release-17.09", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
