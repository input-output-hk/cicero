{ self, ... }@args:

let
  inherit (self.inputs.nixpkgs) lib;
  inherit (self.lib) std;

  wfLib = import ../workflows-lib.nix self;

  nixpkg = pkg:
    "github:NixOS/nixpkgs/${
      self.inputs.nixpkgs.rev or "nixpkgs-unstable"
    }#${pkg}";
  ciceropkg = pkg: "github:input-output-hk/cicero/${self.rev or ""}#${pkg}";

  simple = [ wfLib.jobDefaults std.singleTask ];
  setup = pr: with std; [
    (lib.optionalAttrs (pr ? statuses_url) (github.reportStatus pr.statuses_url))
    (git.clone pr.head)
  ];
in std.callWorkflow args {
  actions = {
    pr = { pr ? null, github-event ? { } }: {
      when = {
        "not yet started" = pr == null;
        "GitHub event is a PR event" = github-event ? pull_request;
      };

      success.pr =
        # TODO make it possible to drop `or null` using lazier evaluation
        github-event.pull_request or null;

      job = with std;
        simple ++ [
          (script "bash" ''
            echo 'TODO make it possible to omit would-be no-op jobs'
          '')
        ];
    };

    gocritic = { pr ? null, gocritic ? null }: {
      when = {
        "PR received" = pr != null;
        "gocritic hasn't run yet" = gocritic == null;
      };

      job = with std;
        simple ++ (setup pr) ++ [
          {
            resources.memory = 1024;
            config.packages =
              data-merge.append (map ciceropkg [ "gocritic" "go" ]);
          }
          (script "bash" ''
            gocritic check -enableAll ./...
          '')
        ];
    };

    nixfmt = { pr ? null, nixfmt ? null }: {
      when = {
        "PR received" = pr != null;
        "nixfmt hasn't run yet" = nixfmt == null;
      };

      job = with std;
        simple ++ (setup pr) ++ [
          {
            resources.memory = 2 * 1024;
            config.packages = data-merge.append (map nixpkg [ "fd" "nixfmt" ]);
          }
          (script "bash" ''
            fd -e nix -X nixfmt -c
          '')
        ];
    };

    build = { pr ? null, gocritic ? null, nixfmt ? null, build ? null }: {
      when = {
        "gocritic passes" = gocritic;
        "nixfmt passes" = nixfmt;
        "build hasn't run yet" = build == null;
      };

      job = with std;
        simple ++ (setup pr) ++ [
          {
            resources = {
              memory = 4 * 1024;
              cpu = 16000;
            };
            config.packages = data-merge.append
              [ "github:input-output-hk/nomad-driver-nix/wrap-nix#wrap-nix" ];
          }
          (script "bash" ''
            echo "nameserver ''${NAMESERVER:-1.1.1.1}" > /etc/resolv.conf

            export NIX_CONFIG="
            experimental-features = nix-command flakes
            "

            nix build
          '')
        ];
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

      job = with std;
        simple ++ (setup pr) ++ [
          {
            resources.memory = 1024;
            config.packages = data-merge.append (map nixpkg [ "cue" "nomad" ]);
          }
          (script "bash" ''
            cue export ./jobs -e jobs.cicero \
              ${
                lib.optionalString (environment != null)
                "-t env=${lib.escapeShellArg environment}"
              } \
              -t 'sha=${pr.head.sha}' \
              > job.json
            nomad run job.json
          '')
        ];
    };
  };
}
