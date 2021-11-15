{ id, ... }:

let lib = import ../workflows-lib.nix;
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
                "github:nixos/nixpkgs/nixpkgs-unstable#bash"
                "github:nixos/nixpkgs/nixpkgs-unstable#jq"
                "github:nixos/nixpkgs/nixpkgs-unstable#curl"
              ];
              command = [ "/bin/trigger" "--config" "/local/trigger.yaml" ];
            };

            Templates = [{
              DestPath = "local/trigger.yaml";
              LeftDelim = "";
              RightDelim = "";
              EmbeddedTmpl = builtins.toJSON {
                settings = {
                  host = "0.0.0.0:4567";
                  print_commands = true;
                  capture_output = false;
                  secret = "TODO get from vault";
                };
                events = rec {
                  common = ''
                    set -exuo pipefail
                    function prop {
                        <<< '{payload}' jq -r "$1"
                    }
                  '';
                  pull_request = ''
                    case $(prop .action) in
                        opened | reopened | synchronize ) ;;
                        * ) exit 0 ;;
                    esac

                    if [[ $(prop .pull_request.base.ref) != $(prop .repository.default_branch) ]]; then
                        >&2 echo 'Ignoring event: PR base is not the default branch. This could allow arbitrary code execution.'
                        exit
                    fi

                    <<< '{payload}' jq -r '{
                        Source: "github:\(.repository.full_name)/\(.pull_request.base.sha)",
                        Inputs: {"github-event": .}
                    }' | curl "$CICERO_API_URL"/workflow/instance/ --data-binary @-
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
