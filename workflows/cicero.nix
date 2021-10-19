{ id, workflow }:
let
  hasCloneUrl = pr: pr ? repository ? clone_url != false;
  hasGitHash = pr: pr ? sha != false;

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
        "pr.repository.clone_url exists" = hasCloneUrl pr;
        "pr.sha exists" = hasGitHash pr;
      };

      run = ''
        ${install [ "github:input-output-hk/cicero#gocritic" ]}

        ${clone pr}

        gocritic check -enableAll ./...
      '';
    };

    nixfmt = { pr ? { } }: {
      when = {
        "pr.repository.clone_url exists" = hasCloneUrl pr;
        "pr.sha exists" = hasGitHash pr;
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
