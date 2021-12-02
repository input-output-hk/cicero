{ self, ... } @ args:

let
  inherit (self.inputs.nixpkgs) lib;
  inherit (self.lib) std;

  wfLib = import ../workflows-lib.nix self;

  pkg = pkg: "github:NixOS/nixpkgs/${self.inputs.nixpkgs.rev or "nixpkgs-unstable"}#${pkg}";
in

std.callWorkflow args {
  actions = {
    github = { sha ? null, environment ? null, github ? null }: {
      when = {
        "sha given" = sha != null;
        "has not run yet" = github == null;
      };

      job = with std; [
        wfLib.jobDefaults
        singleTask
        (git.clone {
          repo.clone_url = "https://github.com/input-output-hk/cicero";
          inherit sha;
        })
        {
          resources.memory = 1024;
          config.packages = data-merge.append (map pkg [
            "cue"
            "nomad"
          ]);
        }
        (script "bash" ''
          cue export ./jobs -e jobs.webhooks \
            ${lib.optionalString (environment != null) "-t env=${lib.escapeShellArg environment}"} \
            -t sha=${lib.escapeShellArg sha} \
            > job.json
          nomad run job.json
        '')
      ];
    };
  };
}
