self:

let
  inherit (self.inputs.nixpkgs) lib;
  inherit (self.inputs) data-merge;
in

rec {
  workflows = rec {
    mkWorkflow = wf: { name, id } @ info: let
      initWf = wf (wf info // info);
    in initWf // {
      actions = mkActions (initWf.actions (initWf.actions { /* placeholder / no info yet */ }));
    };

    mkActions = lib.mapAttrs mkAction;

    mkAction = name: action:
      let info = { inherit name; }; in
      action (action info {} // info);
  };

  jobs.singleTask = { name, ... }: task: {
    group.${name}.task.${name} = task;
  };

  tasks = {
    chains = {
      chain = steps:
        lib.foldr (a: b:
          if builtins.typeOf a == "set"
          then data-merge.merge b a
          else a b
        ) {} steps;

      /*
        Example:

        ```nix
        wrapScript "bash" (inner: ''
          echo 'Running...'
          time ${lib.escapeShellArgs inner}
          echo 'Finished.'
        '')
        ```
      */
      wrapScript = language: outerFn: inner: let
        outer = script language (outerFn inner.config.command);
      in
        data-merge.merge (lib.recursiveUpdate inner {
          config.command = outer.config.command;
        }) {
          config.packages = data-merge.append outer.config.packages;
          template = data-merge.append outer.template;
        };

      git.clone = { repo, sha, ... }: next:
        data-merge.merge
          (wrapScript "bash" (next: ''
            set -exuo pipefail

            export SSL_CERT_FILE=/current-profile/etc/ssl/certs/ca-bundle.crt
            export HOME="$PWD/.home"

            mkdir -p "$HOME"

            git config --global advice.detachedHead false
            git clone --quiet ${lib.escapeShellArg repo.clone_url} src
            cd src
            git checkout ${lib.escapeShellArg sha}

            ${lib.escapeShellArgs next}
          '') next)
          {
            config.packages = data-merge.append [
              "github:nixos/nixpkgs/nixpkgs-unstable#gitMinimal"
            ];
          };

      github.reportStatus = { statuses_url, workflow, action }: next:
        data-merge.merge
          (wrapScript "bash" (next: ''
            set -euxo pipefail

            function report {
              cicero-std github status ${lib.escapeShellArg statuses_url} \
                --arg state "$1" \
                --arg description 'Workflow #'${lib.escapeShellArg workflow.id} \
                --arg workflow_id ${lib.escapeShellArg workflow.id} \
                --arg workflow_name ${lib.escapeShellArg workflow.name} \
                --arg action_name ${lib.escapeShellArg action.name}
            }

            function err {
              report error
            }
            trap err ERR

            report pending

            if ${lib.escapeShellArgs next}; then
              report success
            else
              report failure
            fi
          '') next)
          {
            config.packages = data-merge.append [
              "github:input-output-hk/cicero/${self.rev or ""}#cicero-std"
            ];
          };
    };

    script = language: script: let
      runner = "run-${language}";
      scriptName = builtins.hashString "md5" script;
    in {
      config = {
        packages = [ "github:input-output-hk/cicero/${self.rev or ""}#${runner}" ];
        command = [
          # It is ok to hard-code the system here
          # because we only care about the derivation name.
          "/bin/${
            self.outputs.legacyPackages.x86_64-linux.${runner}.name
          }"
          "/local/scripts/${language}/${scriptName}"
        ];
      };

      template = [ {
        destination = "local/scripts/${language}/${scriptName}";
        left_delimiter = "";
        right_delimiter = "";
        data = script;
      } ];
    };
  };

  inherit (workflows) mkWorkflow;
  inherit (jobs) singleTask;
  inherit (tasks) script;
  inherit (tasks.chains) chain wrapScript git github;
  inherit data-merge;
}
