{ id, run }:

{
  actions = {
    github = { }: {
      job = (import ../workflows-nomad.nix) // {
        TaskGroups = [{
          Name = "prs";

          Tasks = [{
            Name = "prs";

            Config = {
              flake = "github:input-output-hk/cicero#webhook-trigger";
              packages = [
                "github:nixos/nixpkgs/nixpkgs-unstable#jq"
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
                    # liftbridge-cli p -c -s 'workflow.X.start'
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
