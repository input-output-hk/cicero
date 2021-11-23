{ id, run }:

let
  lib = import ../workflows-lib.nix;

  defaultPackages = [
    "github:input-output-hk/cicero#cicero-std"
    "github:nixos/nixpkgs/nixpkgs-unstable#coreutils"
    "github:nixos/nixpkgs/nixpkgs-unstable#gnutar"
    "github:nixos/nixpkgs/nixpkgs-unstable#xz"
    "github:nixos/nixpkgs/nixpkgs-unstable#cacert"
    "github:nixos/nixpkgs/nixpkgs-unstable#gitMinimal"
  ];

  clone = pr: ''
    set -exuo pipefail

    export NIX_CONFIG="experimental-features = nix-command flakes"
    export SSL_CERT_FILE="/current-profile/etc/ssl/certs/ca-bundle.crt"
    export HOME="$PWD/.home"

    mkdir "$HOME"

    git config --global advice.detachedHead false
    git clone --quiet ${pr.head.repo.clone_url} src
    cd src
    git checkout ${pr.head.sha}
  '';

  workflowName = "cicero"; # TODO get passed in from lib.nix?

  reportGithubStatus = pr: actionName: script: ''
    set -euxo pipefail

    function report {
      cicero-std github status ${pr.statuses_url} \
        --arg state "$1" \
        --arg description "Workflow #${id}" \
        --arg workflow_id ${id} \
        --arg workflow_name '${workflowName}' \
        --arg action_name '${actionName}' # FIXME escape
    }

    function err {
      report error
    }
    trap err ERR

    report pending

    if {
      ${script}
    }; then
      report success
    else
      report failure
    fi
  '';
in {
  actions = {
    pr = { pr ? null, github-event ? { } }: {
      when = {
        "not yet started" = pr == null;
        "GitHub event is a PR event" = github-event ? pull_request;
      };
      success.pr =
        github-event.pull_request or null; # TODO make it possible to drop `or null`
      job = lib.addNomadJobDefaults (run "bash" { } ''
        echo 'TODO make it possible to omit would-be no-op jobs?'
      '');
    };

    gocritic = { pr ? null, gocritic ? null }: {
      when = {
        "PR received" = pr != null;
        "gocritic hasn't run yet" = gocritic == null;
      };

      job = lib.addNomadJobDefaults (run "bash" {
        memory = 1024;
        packages = defaultPackages ++ [
          "github:input-output-hk/cicero/69f334ee30ec406bc3a2720b49b7189c2a3b3da1#gocritic"
          "github:input-output-hk/cicero/69f334ee30ec406bc3a2720b49b7189c2a3b3da1#go"
        ];
      } (reportGithubStatus pr "gocritic" ''
        ${clone pr}

        gocritic check -enableAll ./...
      ''));
    };

    nixfmt = { pr ? null, nixfmt ? null }: {
      when = {
        "PR received" = pr != null;
        "nixfmt hasn't run yet" = nixfmt == null;
      };

      job = lib.addNomadJobDefaults (run "bash" {
        memory = 2 * 1024;
        packages = defaultPackages ++ [
          "github:nixos/nixpkgs/nixpkgs-unstable#fd"
          "github:nixos/nixpkgs/nixpkgs-unstable#nixfmt"
        ];
      } (reportGithubStatus pr "nixfmt" ''
        ${clone pr}

        fd -e nix -X nixfmt -c
      ''));
    };

    build = { pr ? null, gocritic ? null, nixfmt ? null, build ? null }: {
      when = {
        "gocritic passes" = gocritic;
        "nixfmt passes" = nixfmt;
        "build hasn't run yet" = build == null;
      };

      job = lib.addNomadJobDefaults (run "bash" {
        memory = 4 * 1024;
        cpu = 16000;
        packages = defaultPackages
          ++ [ "github:input-output-hk/nomad-driver-nix/wrap-nix#wrap-nix" ];
      } (reportGithubStatus pr "build" ''
        ${clone pr}

        echo "nameserver ''${NAMESERVER:-1.1.1.1}" > /etc/resolv.conf
        nix build
      ''));
    };

    deploy = { pr ? null, environment ? null, gocritic ? null, nixfmt ? null
      , build ? null, deploy ? null }: {
        when = {
          "build passes" = build;
          "deploy hasn't run yet" = deploy == null;
        };

        job = lib.addNomadJobDefaults (run "bash" {
          memory = 1024;
          packages = defaultPackages ++ [
            "github:nixos/nixpkgs/nixpkgs-unstable#cue"
            "github:nixos/nixpkgs/nixpkgs-unstable#nomad"
          ];
        } (reportGithubStatus pr "deploy" ''
          ${clone pr}

          cue export ./jobs -e jobs.cicero \
            ${if environment == null then "" else "-t env=${environment}"} \
            -t 'sha=${pr.head.sha}' \
            > job.json
          nomad run job.json
        ''));
      };
  };
}
