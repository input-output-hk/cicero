{ id, ... }:

let
  lib = import ../workflows-lib.nix;
in {
  actions = {
    github = { }: {
      job = lib.addNomadJobDefaults {
        Type = "service";

        TaskGroups = [{
          Name = "webhooks";

          Tasks = [{
            Name = "webhooks";

            Config = {
              packages = [
                "github:input-output-hk/cicero#webhook-trigger"
                "github:input-output-hk/cicero#cicero-evaluator-nix"
                "github:nixos/nixpkgs/nixpkgs-unstable#jq"
                "github:nixos/nixpkgs/nixpkgs-unstable#curl"
              ];
              command = [ "/bin/trigger" "--config" "/local/trigger.yaml" ];
            };

            Templates = [{
              DestPath = "local/trigger.yaml";
              # EmbeddedTempl = "\${meta.triggerConfig}";
              LeftDelim = "";
              RightDelim = "";
              EmbeddedTmpl = builtins.toJSON {
                settings.host = "0.0.0.0:4567";
                events = rec {
                  common = ''
                    set -exuo pipefail
                    function prop {
                        <<< '{payload}' jq -r "$1"
                    }
                  '';
                  pull_request = ''
                    case '{action}' in
                        opened | reopened | synchronize ) ;;
                        * ) exit 0 ;;
                    esac

                    export CICERO_WORKFLOW_SRC=github:$(prop .pull_request.base.repo.full_name)/$(prop .pull_request.base.sha)

                    readarray -t names <<< $(cicero-evaluator-nix list | jq -r .[])
                    for name in "''${names[@]}"; do
                        export name
                        <<< '{payload}' \
                        jq -r '{Source: env.CICERO_WORKFLOW_SRC, Name: env.name, Inputs: {pr: .}}' \
                        | curl "$CICERO_API_URL"/workflow/instance/ --data-binary @-
                    done
                  '';
                };
              };
            }];
          }];
        }];
      };
    };
  };
}
