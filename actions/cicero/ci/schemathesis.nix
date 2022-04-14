{
  name,
  std,
  ...
} @ args: {
  inputs.start = ''
    "cicero/ci": start: {
      clone_url: string
      sha: string
      statuses_url?: string

      ref?: "refs/heads/\(default_branch)"
      default_branch?: string
    }
  '';

  job = {start}: let
    cfg = start.value."cicero/ci".start;
  in
    std.chain args [
      (std.escapeNames [] [])

      {
        ${name}.group.schemathesis = {
          network.port.cicero = {};

          task = {
            cicero = {
              lifecycle = {
                hook = "prestart";
                sidecar = true;
              };

              config.nixos = "github:input-output-hk/cicero/${cfg.sha}#nixosConfigurations.cicero";

              template = [
                {
                  destination = "/etc/cicero/start.args";
                  data = ''
                    --web-listen :{{ env "NOMAD_PORT_cicero" }}

                    ${
                      ""
                      /*
                       Do not try to connect to Nomad
                       as we have none running.
                       */
                    }
                    web
                  '';
                }
              ];
            };

            schemathesis = std.chain args [
              (std.github.reportStatus cfg.statuses_url or null)

              (std.git.clone cfg)

              {
                resources = {
                  cpu = 3000;
                  memory = 1024;
                };
              }

              std.nix.develop

              (std.script "bash" ''
                exec schemathesis run http://127.0.0.1:$NOMAD_PORT_cicero/documentation/cicero.json --validate-schema=false
              '')
            ];
          };
        };
      }
    ];
}
