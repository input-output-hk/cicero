{inputs, ...}: {
  imports = with inputs; [
    tullia.flakePartsModules.default
  ];

  perSystem = {inputs', ...}: {
    tullia = let
      ciInputName = "GitHub Push or PR";
      repository = "input-output-hk/cicero";
    in {
      tasks = let
        common = {
          config,
          lib,
          ...
        }: {
          preset = {
            nix.enable = true;

            github.ci = __mapAttrs (_: lib.mkDefault) {
              enable = config.actionRun.facts != {};
              inherit repository;
              remote = config.preset.github.lib.readRepository ciInputName "";
              revision = config.preset.github.lib.readRevision ciInputName "";
            };
          };

          nomad.driver = "exec";
        };
      in {
        lint = {...}: {
          imports = [common];

          config = {
            command.text = ''
              nix develop -L -c lint
            '';

            memory = 1024 * 2;
            nomad.resources.cpu = 1000;
          };
        };

        build = {config, ...}: {
          imports = [common];

          config = {
            after = ["lint"];

            command.text = config.preset.github.status.lib.reportBulk {
              bulk.text = "nix eval .#packages --apply __attrNames --json | nix-systems -i";
              each.text = ''
                nix build -L .#packages."$1".default

                readarray -t tests < <(
                  nix eval .#packages."$1".default.passthru.tests --apply __attrNames --json |
                  jq --raw-output .[]
                )

                for test in "''${tests[@]}"; do
                  installables+=(.#packages."$1".default.passthru.tests."$test")
                done

                if [[ ''${#tests[@]} -gt 0 ]]; then
                  nix build -L "''${installables[@]}"
                fi
              '';
            };

            env.NIX_CONFIG = ''
              extra-system-features = kvm
            '';

            memory = 1024 * 3;
            nomad.resources.cpu = 3500;
          };
        };
      };

      actions."cicero/ci" = {
        task = "build";
        io = ''
          let github = {
            #input: "${ciInputName}"
            #repo: "${repository}"
          }

          #lib.merge
          #ios: [
            #lib.io.github_push & github & {#default_branch: true},
            #lib.io.github_pr   & github,
          ]
        '';
      };
    };
  };
}
