{ name, std, lib, actionLib, ... }@args:

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
            port = {
              http.to = 8080;
              db.to = 5432;
            };
          };

          task = {
            lintAndBuild = std.chain args [
              (lib.optionalAttrs (cfg ? statuses_url)
                (std.github.reportStatus cfg.statuses_url))

              (std.networking.addNameservers [ "1.1.1.1" ])

              (std.git.clone cfg)

              {
                resources.memory = 1024 * 3;

                config.packages = std.data-merge.append [
                  "github:input-output-hk/cicero/${cfg.sha}#devShell.x86_64-linux"
                ];
              }

              (std.wrapScript "bash" (next: ''
                set -ex
                go generate ./...
                lint
                ${lib.escapeShellArgs next}
              ''))

              std.nix.build
            ];

            cicero = {
              lifecycle = {
                hook = "prestart";
                sidecar = true;
              };

              config = {
                packages = [
                  "github:input-output-hk/cicero/${cfg.sha}#cicero-entrypoint"
                ];
                command = [ "/bin/entrypoint" ];
              };

              env.DATABASE_URL = "postgres://cicero:@127.0.0.1:\${NOMAD_PORT_db}/cicero?sslmode=disable";
            };

            dev = {
              lifecycle = {
                hook = "prestart";
                sidecar = true;
              };

              config.nixos = "github:input-output-hk/cicero/${cfg.sha}#nixosConfigurations.dev";
            };

            schemathesis = std.chain args [
              (lib.optionalAttrs (cfg ? statuses_url)
                (std.github.reportStatus cfg.statuses_url))

              (std.networking.addNameservers [ "1.1.1.1" ])

              (std.git.clone cfg)

              {
                config.packages = std.data-merge.append [
                  "github:input-output-hk/cicero/${cfg.sha}#devShell.x86_64-linux"
                ];
              }

              (std.script "bash" ''
                exec schemathesis run http://127.0.0.1:$NOMAD_PORT_http/documentation/cicero.yaml --validate-schema=false
              '')
            ];
          };
        };
      }
    ];
}
