{ name, std, lib, actionLib, ... } @ args:

std.behavior.onInputChange "start" name args

{
  inputs.start = ''
    "${name}": start: {
      clone_url: string
      sha: string
      statuses_url?: string
    }
  '';

  job = { start }:
    let cfg = start.value.${name}.start; in
    std.chain args [
      actionLib.jobDefaults

      (std.escapeNames [ ] [ ])

      {
        cicero-ci.group.cicero = {
          network = {
            mode = "bridge";
            port.http.to = 8080;
          };

          task = {
            lintAndBuild = std.chain args [
              (lib.optionalAttrs (cfg ? statuses_url)
                (std.github.reportStatus cfg.statuses_url))

              (std.git.clone cfg)

              {
                resources = {
                  memory = 4 * 1024;
                  cpu = 16000;
                };
              }

              std.nix.develop
              (std.wrapScript "bash" (next: ''
                lint
                ${lib.escapeShellArg next}
              ''))

              std.nix.build
            ];

            cicero = {
              config.nixos = "github:input-output-hk/cicero/${cfg.sha}#dev";

              lifecycle = {
                hook = "prestart";
                sidecar = true;
              };
            };

            schemathesis = std.chain args [
              (lib.optionalAttrs (cfg ? statuses_url)
                (std.github.reportStatus cfg.statuses_url))

              (std.git.clone cfg)

              std.nix.develop

              (std.script "bash" ''
                schemathesis run http://localhost:$NOMAD_PORT_http/documentation/cicero.yaml --validate-schema=false
              '')
            ];
          };
        };
      }
    ];
}
