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
    let cfg = start.value."${name}".start;
    in
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

              (std.wrapScript "bash" (next: ''
                set -x
                mkdir -p /etc
                echo "nameserver 1.1.1.1" >> /etc/resolv.conf
                ${lib.escapeShellArgs next}
              ''))

              (std.git.clone cfg)

              {
                resources = {
                  memory = 32 * 1024;
                  cpu = 16000;
                };

                config.packages = std.data-merge.append [
                  "github:input-output-hk/cicero#devShell.x86_64-linux"
                ];
              }

              (std.wrapScript "bash" (next: ''
                set -exuo pipefail

                golangci-lint run
                gocritic check -enableAll -disable=ifElseChain,ptrToRefParam,unnamedResult,appendAssign ./...
                nixpkgs-fmt --check .

                ${lib.escapeShellArgs next}
              ''))

              std.nix.build
            ];

            dev = {
              config.nixos =
                "github:input-output-hk/cicero/${cfg.sha}#nixosConfigurations.dev";

              lifecycle = {
                hook = "prestart";
                sidecar = true;
              };
            };

            schemathesis = std.chain args [
              (lib.optionalAttrs (cfg ? statuses_url)
                (std.github.reportStatus cfg.statuses_url))

              (std.wrapScript "bash" (next: ''
                set -ex
                mkdir -p /etc
                echo "nameserver 1.1.1.1" >> /etc/resolv.conf
                exec ${lib.escapeShellArgs next}
              ''))

              (std.git.clone cfg)

              {
                resources = {
                  memory = 32 * 1024;
                  cpu = 16000;
                };

                config.packages = std.data-merge.append [
                  "github:input-output-hk/cicero#devShell.x86_64-linux"
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
