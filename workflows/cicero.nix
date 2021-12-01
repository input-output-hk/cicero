{ std, lib } @ args:

let
  wfLib = import ../workflows-lib.nix args;

  defaultPackages = [
    "github:input-output-hk/cicero#cicero-std"
    "github:nixos/nixpkgs/nixpkgs-unstable#coreutils"
    "github:nixos/nixpkgs/nixpkgs-unstable#gnutar"
    "github:nixos/nixpkgs/nixpkgs-unstable#xz"
    # "github:nixos/nixpkgs/nixpkgs-unstable#cacert" # TODO does having this automatically set SSL_CERT_FILE?
  ];
in

{
  actions = {
    /*
    pr = action: { pr ? null, github-event ? { } }: {
      when = {
        "not yet started: ${workflow.name}/${toString workflow.id}" = pr == null;
        "GitHub event is a PR event" = github-event ? pull_request;
      };
      success.pr =
        github-event.pull_request or null; # TODO make it possible to drop `or null`
      job = wfLib.addNomadJobDefaults (run "bash" { } ''
        echo 'TODO make it possible to omit would-be no-op jobs?'
      '');
    };
    */

    gocritic = { pr ? null, gocritic ? null }: {
      when = {
        # "PR received >${action.actions.workflow.name}/${toString action.actions.workflow.id}#${action.name}<" = pr != null;
        "PR received" = pr != null;
        "gocritic hasn't run yet" = gocritic == null;
      };

      job = with std; [
        wfLib.jobDefaults
        singleTask
        {
          resources.memory = 1024;
          config.packages = data-merge.append (defaultPackages ++ [
            "github:input-output-hk/cicero/69f334ee30ec406bc3a2720b49b7189c2a3b3da1#gocritic"
            "github:input-output-hk/cicero/69f334ee30ec406bc3a2720b49b7189c2a3b3da1#go"
          ]);
        }
        (github.reportStatus pr.statuses_url)
        (git.clone pr.head)
        (script "bash" ''
          gocritic check -enableAll ./...
        '')
      ];
    };

    /*
    nixfmt = { pr ? null, nixfmt ? null }: {
      when = {
        "PR received" = pr != null;
        "nixfmt hasn't run yet" = nixfmt == null;
      };

      job = wfLib.addNomadJobDefaults (run "bash" {
        memory = 2 * 1024;
        packages = defaultPackages ++ [
          "github:nixos/nixpkgs/nixpkgs-unstable#fd"
          "github:nixos/nixpkgs/nixpkgs-unstable#nixfmt"
        ];
        # XXX currently required to show logs in UI
        group = "nixfmt";
        task = "nixfmt";
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

      job = wfLib.addNomadJobDefaults (run "bash" {
        memory = 4 * 1024;
        cpu = 16000;
        packages = defaultPackages
          ++ [ "github:input-output-hk/nomad-driver-nix/wrap-nix#wrap-nix" ];
        # XXX currently required to show logs in UI
        group = "build";
        task = "build";
      } (reportGithubStatus pr "build" ''
        ${clone pr}

        echo "nameserver ''${NAMESERVER:-1.1.1.1}" > /etc/resolv.conf
        nix build
      ''));
    };

    deploy = { pr ? null, environment ? null, build ? null, deploy ? null }: {
      when = {
        # TODO This action is not ready as is and should be split into two.
        # For PRs, it should spin up a dev instance, see if it becomes healthy,
        # maybe run a simple workflow like ping_pong on it,
        # and finally shut it down again.
        # Actual deployments should not be done for PRs
        # but only on merges into master or some other branch.
        never = false;

        "build passes" = build;
        "deploy hasn't run yet" = deploy == null;
      };

      job = wfLib.addNomadJobDefaults (run "bash" {
        memory = 1024;
        packages = defaultPackages ++ [
          "github:nixos/nixpkgs/nixpkgs-unstable#cue"
          "github:nixos/nixpkgs/nixpkgs-unstable#nomad"
        ];
        # XXX currently required to show logs in UI
        group = "deploy";
        task = "deploy";
      } (reportGithubStatus pr "deploy" ''
        ${clone pr}

        cue export ./jobs -e jobs.cicero \
          ${if environment == null then "" else "-t env=${environment}"} \
          -t 'sha=${pr.head.sha}' \
          > job.json
        nomad run job.json
      ''));
    };
    */
  };
}
