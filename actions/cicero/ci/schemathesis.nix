{ name, std, lib, actionLib, ... }@args:

{
  inputs.start = ''
    "cicero/ci": start: {
      clone_url: string
      sha: string
      statuses_url?: string
    }
  '';

  job = { start }:
    let cfg = start.value."cicero/ci".start; in
    std.chain args [
      actionLib.jobDefaults

      (std.escapeNames [ ] [ ])

      {
        ${name}.group.schemathesis = {
          network = {
            mode = "bridge";
            port = {
              http.to = 8080;
              db.to = 5432;
            };
          };

          task = {
            dev = {
              lifecycle = {
                hook = "prestart";
                sidecar = true;
              };

              config.nixos = "github:input-output-hk/cicero/${cfg.sha}#nixosConfigurations.dev";
            };

            cicero = {
              lifecycle.sidecar = true;

              config = {
                packages = [ "github:input-output-hk/cicero/${cfg.sha}#cicero-entrypoint" ];
                command = [ "/bin/entrypoint" "--web-listen" "\${NOMAD_PORT_http}" ];
              };

              env.DATABASE_URL = "postgres://cicero:@127.0.0.1:\${NOMAD_PORT_db}/cicero?sslmode=disable";
            };

            schemathesis = std.chain args [
              { lifecycle.hook = "poststart"; }

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
