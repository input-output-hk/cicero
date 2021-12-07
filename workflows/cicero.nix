{ self, ... }@args:

let
  inherit (self.inputs.nixpkgs) lib;
  inherit (self.lib) std;

  wfLib = import ../workflows-lib.nix self;

  common = pr: [
    wfLib.jobDefaults
    std.singleTask
    (lib.optionalAttrs (pr ? statuses_url)
      (std.github.reportStatus pr.statuses_url))
    (std.git.clone pr.head)
  ];
in std.callWorkflow args {
  actions = {
    pr = { pr ? null, github-event ? { } }: {
      when = {
        "not yet started" = pr == null;
        "GitHub event is a PR event" = github-event ? pull_request;
      };

      success.pr = github-event.pull_request or null;
    };

    gocritic = { pr ? null, gocritic ? null }: {
      when = {
        "PR received" = pr != null;
        "gocritic hasn't run yet" = gocritic == null;
      };

      job = (common pr) ++ [
        { resources.memory = 1024; }
        std.nix.develop
        (std.script "bash" ''
          gocritic check -enableAll ./...
        '')
      ];
    };

    golangci-lint = { pr ? null, golangci-lint ? null }: {
      when = {
        "PR received" = pr != null;
        "golangci-lint hasn't run yet" = golangci-lint == null;
      };

      job = (common pr) ++ [
        { resources.memory = 1024; }
        std.nix.develop
        (std.script "bash" ''
          golangci-lint run
        '')
      ];
    };

    nixfmt = { pr ? null, nixfmt ? null }: {
      when = {
        "PR received" = pr != null;
        "nixfmt hasn't run yet" = nixfmt == null;
      };

      job = (common pr) ++ [
        { resources.memory = 2 * 1024; }
        std.nix.develop
        (std.script "bash" ''
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

      job = (common pr) ++ [
        (std.wrapScript "bash" (next: ''
          echo "nameserver ''${NAMESERVER:-1.1.1.1}" > /etc/resolv.conf
          ${lib.escapeShellArgs next}
        ''))
        {
          resources = {
            memory = 4 * 1024;
            cpu = 16000;
          };
        }
        std.nix.build
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

      job = (common pr) ++ [
        { resources.memory = 1024; }
        std.nix.develop
        (std.script "bash" ''
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
