{ id, run }:

{
  actions = {
    github = { }: {
      job = (import ../workflows-nomad.nix) // {
        TaskGroups = [{
          Name = "webhooks";

          Tasks = [{
            Name = "webhooks";

            Config = {
              packages = [
                "github:input-output-hk/cicero#webhook-trigger"
                "github:nixos/nixpkgs/nixpkgs-unstable#jq"
                "github:nixos/nixpkgs/nixpkgs-unstable#curl"
              ];
              command = "/bin/trigger " + builtins.toFile "trigger.yaml" (builtins.toJSON {
                settings.host = "0.0.0.0:4567";
                events = rec {
                  common = ''
                    set -exuo pipefail
                    function prop {
                        <<<'{payload}' jq --raw-output "$1"
                    }
                  '';
                  pull_request = ''
                    case '{action}' in
                        opened | reopened | synchronize ) ;;
                        * ) exit 0 ;;
                    esac
                    body=$(<<<'{payload}' jq '.pull_request | {source: "github:\(.head.repo.full_name)"}')
                    curl -X POST http://127.0.0.1/api/workflow/ --data "$body"
                  '';
                };
              });
            };
          }];
        }];
      };
    };
  };
}
