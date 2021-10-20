{ id, workflow }:

let
  length = arg: builtins.length (builtins.attrNames arg);
  any = arg: (length arg) > 0;

  install = ''
    set -exuo pipefail

    export HOME=.home
    mkdir $HOME

    export NIX_CONFIG="
    experimental-features = nix-command flakes
    "

    nix profile install github:NixOS/nixpkgs/364b5555ee04bf61ee0075a3adab4c9351a8d38c#hyperspace-cli --profile local
    nix profile install github:NixOS/nixpkgs/364b5555ee04bf61ee0075a3adab4c9351a8d38c#util-linux --profile local

    export PATH="$PATH:$PWD/local/bin"
  '';
in

workflow {
  name = "github";

  version = 0;

  steps = {
    clone = { pr ? {}, src-hyper ? "" }: {
      when = {
        "we got a PR" = any pr;
        "missing hyper" = src-hyper == "";
      };

      type = "bash";

      job = ''
        git clone ${pr.repository.clone_url} src

        ${install}

        uuid=$(uuidgen)
        (tar cJf - src | hyp beam "$uuid") &
        liftbridge-cli p -s workflow.github.${id}.cert -c -m "{\"src-hyper\":\"$uuid\"}"
        wait
      '';
    };

    checkout = { pr ? {}, src-hyper ? "" }: {
      when = {
        "we got a PR" = any pr;
        "repository was cloned" = src-hyper != "";
      };

      success = {
        checkout = true;
        sha = pr.commit.sha or "<unknown>";
      };

      type = "bash";

      job = ''
        ${install}
        hyp beam ${src-hyper} > src.tar.xz
        tar xJf src.tar.xz src

        cd src
        git remote update
        git checkout ${pr.commit.sha}
      '';
    };

    test = { pr ? {}, test ? false, checkout ? false, sha ? null }: {
      when = {
        "we got a PR" = any pr;
        "is checked out" = checkout;
        "test didn't run yet" = !test;
        "received a new commit" = sha != null;
      };

      type = "bash";

      job = ''
        cd ${pr.repository.full_name}.${id}
        git checkout ${pr.commit.sha}
      '';
    };
  };
}
