{ id, run }:

let
  hasCloneUrl = pr: pr.repository.clone_url or false != false;
  hasGitHash = pr: pr ? sha;

  install = pkgs:
    let
      profileInstall = builtins.concatStringsSep "\n"
        (map (pkg: "nix profile install ${pkg} --profile local") pkgs);
    in ''
      export NIX_CONFIG="
      experimental-features = nix-command flakes
      "

      ${profileInstall}

      export PATH="$PATH:$PWD/local/bin"
    '';

  clone = pr: ''
    set -exuo pipefail

    export HOME=.home
    mkdir $HOME

    git clone ${pr.repository.clone_url} src
    cd src
    git checkout ${pr.sha}
  '';

in {
  version = 0;

  actions = {
    gocritic = { pr ? { } }: {
      when = {
        "pr.repository.clone_url exists" = hasCloneUrl pr;
        "pr.sha exists" = hasGitHash pr;
      };

      job = (run "bash" ''
        ${install [ "github:input-output-hk/cicero#gocritic" ]}

        ${clone pr}

        gocritic check -enableAll ./...
      '') // (import ../workflows-nomad.nix);
    };

    nixfmt = { pr ? { } }: {
      when = {
        "pr.repository.clone_url exists" = hasCloneUrl pr;
        "pr.sha exists" = hasGitHash pr;
      };

      job = (run "bash" ''
        ${install [
          "github:nixos/nixpkgs/nixos-21.05#nixfmt"
          "github:nixos/nixpkgs/nixos-21.05#fd"
        ]}

        ${clone pr}

        fd -e nix -X nixfmt -c
      '') // (import ../workflows-nomad.nix);
    };
  };
}
