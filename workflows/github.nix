{ id, run }:

let
  length = arg: builtins.length (builtins.attrNames arg);
  any = arg: (length arg) > 0;

  bash = script:
    run "bash" ((import ../workflows-nomad.nix) // {
      packages = [
        "github:NixOS/nixpkgs/364b5555ee04bf61ee0075a3adab4c9351a8d38c#hyperspace-cli"
        "github:NixOS/nixpkgs/364b5555ee04bf61ee0075a3adab4c9351a8d38c#util-linux"
      ];
    }) script;

in {
  version = 0;

  actions = {
    clone = { pr ? { }, src-hyper ? "" }: {
      when = {
        "we got a PR" = any pr;
        "missing hyper" = src-hyper == "";
      };

      job = bash ''
        set -exuo pipefail

        export HOME="$PWD/.home"
        mkdir "$HOME"

        git clone ${pr.repository.clone_url} src

        uuid=$(uuidgen)
        (tar cJf - src | hyp beam "$uuid") &
        liftbridge-cli p -s workflow.github.${id}.cert -c -m "{\"src-hyper\":\"$uuid\"}"
        wait
      '';
    };

    checkout = { pr ? { }, src-hyper ? "" }: {
      when = {
        "we got a PR" = any pr;
        "repository was cloned" = src-hyper != "";
      };

      success = {
        checkout = true;
        sha = pr.commit.sha or "<unknown>";
      };

      job = bash ''
        hyp beam ${src-hyper} > src.tar.xz
        tar xJf src.tar.xz src

        cd src
        git remote update
        git checkout ${pr.commit.sha}
      '';
    };

    test = { pr ? { }, test ? false, checkout ? false, sha ? null }: {
      when = {
        "we got a PR" = any pr;
        "is checked out" = checkout;
        "test didn't run yet" = !test;
        "received a new commit" = sha != null;
      };

      job = bash ''
        cd ${pr.repository.full_name}.${id}
        git checkout ${pr.commit.sha}
      '';
    };
  };
}
