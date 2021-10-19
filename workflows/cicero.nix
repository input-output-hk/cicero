{ id, workflow }:
let
  hasAttrByPath = attrPath: e:
    let attr = builtins.head attrPath;
    in if attrPath == [ ] then
      true
    else if e ? ${attr} then
      hasAttrByPath (builtins.tail attrPath) e.${attr}
    else
      false;

  install = pkgs:
    builtins.concatStringsSep "\n"
    (map (pkg: "nix profile install ${pkg} --profile local") pkgs);

  clone = pr: ''
    set -exuo pipefail

    export HOME=.home
    mkdir $HOME

    export NIX_CONFIG="
    experimental-features = nix-command flakes
    "

    nix profile install github:input-output-hk/cicero#gocritic --profile local

    export PATH="$PATH:$PWD/local/bin"

    git clone ${pr.repository.clone_url} src
    cd src
    git checkout ${pr.sha}
  '';
in workflow {
  name = "cicero";

  version = 0;

  tasks = {
    gocritic = { pr ? { } }: {
      when = {
        "pr.repository.clone_url exists" =
          hasAttrByPath [ "repository" "clone_url" ];
        "pr.sha exists" = hasAttrByPath [ "sha" ];
      };

      run = ''
        ${install [ "github:input-output-hk/cicero#gocritic" ]}

        ${clone pr}

        gocritic check -enableAll ./...
      '';
    };

    nixfmt = { pr ? { } }: {
      when = {
        "pr.repository.clone_url exists" =
          hasAttrByPath [ "repository" "clone_url" ];
        "pr.sha exists" = hasAttrByPath [ "sha" ];
      };

      run = ''
        ${install [
          "github:nixos/nixpkgs/nixos-21.05#nixfmt"
          "github:nixos/nixpkgs/nixos-21.05#fd"
        ]}

        ${clone pr}

        fd -e nix -X nixfmt -c
      '';
    };
  };
}
