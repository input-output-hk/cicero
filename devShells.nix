{inputs, ...}: {
  perSystem = {
    config,
    system,
    inputs',
    ...
  }: {
    devShells.default = inputs'.devshell.legacyPackages.mkShell ({
      lib,
      pkgs,
      extraModulesPath,
      ...
    }: {
      imports = map (i: "${extraModulesPath}/${i}.nix") [
        "language/go"
      ];

      language.go.package = pkgs.go_1_18;

      env = [
        {
          name = "CONSOLE_LOGGING_ENABLED";
          value = true;
        }
        {
          name = "DATABASE_URL";
          value = "postgres://postgres:@127.0.0.1:15432/cicero?sslmode=disable";
        }
        {
          name = "LOKI_ADDR";
          value = "http://127.0.0.1:13100";
        }
        {
          name = "VAULT_ADDR";
          value = "http://127.0.0.1:18200";
        }
        {
          name = "NOMAD_ADDR";
          value = "http://127.0.0.1:14646";
        }
        {
          # Put stuff into OCI images built with Tullia.
          name = "CICERO_EVALUATOR_NIX_EXTRA_ARGS";
          value = ''
            {
              rootDir = let
                nixpkgs = builtins.getFlake "github:NixOS/nixpkgs/93950edf017d6a64479069fbf271aa92b7e44d7f";
                pkgs = nixpkgs.legacyPackages.''${system};
              in
                # for transformers
                pkgs.bash;
            }
          '';
        }
        {
          name = "CICERO_EVALUATOR_NIX_OCI_REGISTRY";
          value = "docker://127.0.0.1:15000";
        }
        {
          name = "CICERO_EVALUATOR_NIX_OCI_REGISTRY_SKOPEO_COPY_ARGS";
          value = "--dest-tls-verify=false";
        }
        {
          name = "CICERO_EVALUATOR_NIX_BINARY_CACHE";
          value = "http://127.0.0.1:17745?compression=none";
        }
      ];

      commands = [
        {package = "dbmate";}
        {package = config.packages.schemathesis;}
        {package = "nomad";}
        {package = "vault";}
        {package = "damon";}
        {
          name = "lint";
          command = ''
            golangci-lint run -E gocritic --timeout 5m &&
            nix fmt -- --fail-on-change
          '';
          help = "Run code linters";
        }
        {
          name = "dev-cicero";
          command = ''
            set -x
            dbmate up
            go run . start \
              --log-level trace \
              --victoriametrics-addr http://127.0.0.1:18428 \
              --prometheus-addr http://127.0.0.1:13100 \
              --web-listen :18080 \
              --transform dev-cicero-transformer \
              "$@"
          '';
          help = "Run Cicero from source";
        }
        {
          name = "psqlc";
          command = ''psql -d "$DATABASE_URL" "$@"'';
          help = "psql into Cicero DB";
        }
      ];

      devshell = {
        name = "cicero";
        packages = [
          # cicero
          config.packages.cicero-evaluator-nix
          (
            let
              args = {
                nixpkgsRev = inputs.nixpkgs.rev;
                datacenters = ["dc1"];
                ciceroWebUrl = "http://127.0.0.1:18080";
                nixConfig = ''
                  substituters = http://10.0.2.15:17745?compression=none
                  extra-trusted-public-keys = spongix:yNfB2+pMSmrjNyMRWob1oEs4ihPnVKPkECWiDxv1MNI=
                  post-build-hook = /local/post-build-hook
                '';
                postBuildHook = ''
                  #! /bin/bash
                  set -euf
                  export IFS=' '
                  if [[ -n "$OUT_PATHS" ]]; then
                    echo 'Uploading to cache: '"$OUT_PATHS"
                    exec nix copy --to 'http://10.0.2.15:17745?compression=none' $OUT_PATHS
                  fi
                '';
              };
            in
              pkgs.writers.writeDashBin "dev-cicero-transformer" ''
                ${lib.getExe pkgs.jq} --compact-output \
                  --argjson args ${lib.escapeShellArg (builtins.toJSON args)} \
                  '
                    .job |= (
                      del(.Namespace) |
                      .Datacenters = $args.datacenters |
                      .TaskGroups[]?.Tasks[]? |= (
                        .Env += {
                          CICERO_WEB_URL: $args.ciceroWebUrl,
                          CICERO_API_URL: $args.ciceroWebUrl,
                          NIX_CONFIG: ($args.nixConfig + .NIX_CONFIG),
                        } |
                        .Templates += [{
                          DestPath: "local/post-build-hook",
                          Perms: "544",
                          EmbeddedTmpl: $args.postBuildHook,
                        }]
                      ) |
                      if .Type != null and .Type != "batch" then . else (
                        .TaskGroups[]?.Tasks[]? |= (
                          .Vault.Policies += ["cicero"] |
                          if .Driver != "nix" or .Config?.nixos then . else
                            .Config.packages |=
                              # only add bash if needed to avoid conflicts in profile
                              if any(endswith("#bash") or endswith("#bashInteractive"))
                              then .
                              else . + ["github:NixOS/nixpkgs/\($args.nixpkgsRev)#bash"]
                              end
                          end
                        )
                      ) end
                    )
                  '
              ''
          )

          # go
          "gcc"
          "gocode"
          "gotools"
          "gopls"
          "go-mockery"
          "golangci-lint"
          config.packages.go-critic

          # deployment
          "cue"
          inputs.follower.defaultPackage.${system}
          "postgresql"
          "vector"
          "grafana-loki"

          # tools
          inputs.nixos-shell.defaultPackage.${system}
          "httpie"
          "curlie"
          "diffutils"
          "jq"
          "mdbook"
        ];
      };
    });
  };
}
