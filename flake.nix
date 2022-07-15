{
  description = "Flake for the Cicero framework";

  inputs = {
    devshell.url = "github:numtide/devshell";
    nixos-shell = {
      url = "github:Mic92/nixos-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    inclusive.url = "github:input-output-hk/nix-inclusive";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix.url = "github:NixOS/nix/2.8.0";
    alejandra.url = "github:kamadorueda/alejandra";
    utils.url = "github:numtide/flake-utils";
    driver = {
      url = "github:input-output-hk/nomad-driver-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    follower = {
      url = "github:input-output-hk/nomad-follower";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    data-merge.url = "github:divnix/data-merge";
    poetry2nix = {
      url = "github:nix-community/poetry2nix/fetched-projectdir-test";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    spongix = {
      url = "github:input-output-hk/spongix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        inclusive.follows = "inclusive";
      };
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs-unstable.follows = "nixpkgs";
    };
    nix2container = {
      url = "github:nlewo/nix2container";
      inputs.flake-utils.follows = "utils";
    };
    tullia = {
      url = "github:input-output-hk/tullia";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nix2container.follows = "nix2container";
      };
    };
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    devshell,
    nixos-shell,
    driver,
    follower,
    poetry2nix,
    spongix,
    haskell-nix,
    nix,
    alejandra,
    nix2container,
    tullia,
    ...
  }: let
    supportedSystems = ["x86_64-linux"];
  in
    utils.lib.eachSystem supportedSystems
    (
      system: let
        pkgs = nixpkgs.legacyPackages.${system}.extend (nixpkgs.lib.composeManyExtensions [
          devshell.overlay
          poetry2nix.overlay
          follower.overlay
          spongix.overlay
          nix.overlay
          (final: prev: {
            nixos-shell = nixos-shell.defaultPackage.${prev.system};
            alejandra = alejandra.defaultPackage.${prev.system};
            go = prev.go_1_17;
            gouml = final.callPackage pkgs/gouml.nix {};
            gocritic = final.callPackage pkgs/gocritic.nix {};
            schemathesis = final.callPackage pkgs/schemathesis.nix {};
            treefmt-cue = final.callPackage pkgs/treefmt-cue.nix {};
            dev-cicero-transformer = let
              args = {
                nixpkgsRev = nixpkgs.rev;
                datacenters = ["dc1"];
                ciceroWebUrl = "http://127.0.0.1:18080";
                nixConfig = ''
                  substituters = http://127.0.0.1:17745?compression=none
                  extra-trusted-public-keys = spongix:yNfB2+pMSmrjNyMRWob1oEs4ihPnVKPkECWiDxv1MNI=
                  post-build-hook = /local/post-build-hook
                '';
                postBuildHook = ''
                  #! /bin/bash
                  set -euf
                  export IFS=' '
                  if [[ -n "$OUT_PATHS" ]]; then
                    echo 'Uploading to cache: '"$OUT_PATHS"
                    exec nix copy --to 'http://127.0.0.1:17745?compression=none' $OUT_PATHS
                  fi
                '';
              };
            in
              prev.writers.writeDashBin "dev-cicero-transformer" ''
                jq --compact-output \
                  --argjson args ${nixpkgs.lib.escapeShellArg (builtins.toJSON args)} \
                  '
                    .job[]?.datacenters |= . + $args.datacenters |
                    .job[]?.group[]?.restart.attempts = 0 |
                    .job[]?.group[]?.task[]? |= (
                      .vault.policies |= . + ["cicero"] |
                      .env |= . + {
                        CICERO_WEB_URL: $args.ciceroWebUrl,
                        CICERO_API_URL: $args.ciceroWebUrl,
                        NIX_CONFIG: ($args.nixConfig + .NIX_CONFIG),
                      } |
                      .template |= . + [{
                        destination: "local/post-build-hook",
                        perms: "544",
                        data: $args.postBuildHook,
                      }] |
                      if .driver != "nix" or .config?.nixos then . else
                        .config.packages |=
                          # only add bash if needed to avoid conflicts in profile
                          if any(endswith("#bash") or endswith("#bashInteractive"))
                          then .
                          else . + ["github:NixOS/nixpkgs/\($args.nixpkgsRev)#bash"]
                          end
                      end |
                      # logs do not work with podman driver
                      # TODO remove once fixed
                      if .driver != "podman" then . else
                        .driver = "docker" |
                        .config.image |= ltrimstr("docker://")
                      end
                    )
                  '
              '';
          })
          self.overlay
        ]);
        selfPkgs = self.packages.${system};
      in
        {
          packages = self.overlay (pkgs // selfPkgs) pkgs;
          defaultPackage = selfPkgs.cicero;
          hydraJobs = selfPkgs;
          devShell = pkgs.devshell.fromTOML ./devshell.toml;
          devShells.cicero-api = selfPkgs.cicero-api.project.shell;
          inherit (selfPkgs.cicero-api) apps;

          # We cannot use pkgs.nixosTest because
          # it gives us no way to inject specialArgs
          # and _module.args lead to infinite recursion with self.
          checks.schemathesis =
            (import "${nixpkgs}/nixos/lib/testing-python.nix" {
              inherit system pkgs;
              extraConfigurations = [{nixpkgs = {inherit pkgs;};}];
              specialArgs = {inherit self;};
            })
            .makeTest {
              name = "schemathesis";
              nodes.main = {pkgs, ...}: {
                imports = [nixos/configs/cicero.nix];

                environment = {
                  # Do not try to connect to Nomad as we have none running.
                  etc."cicero/start.args".text = "web";

                  systemPackages = [pkgs.schemathesis];
                };
              };
              testScript = ''
                main.wait_for_unit("cicero")
                main.succeed("schemathesis run http://127.0.0.1:8080/documentation/cicero.json --validate-schema=false --hypothesis-suppress-health-check=too_slow")
              '';
            };
        }
        // tullia.fromSimple system {
          tasks = import tullia/tasks.nix self;
          actions = import tullia/actions.nix;
        }
    )
    // {
      overlay = final: prev:
        {
          cicero = prev.callPackage pkgs/cicero {flake = self;};
          cicero-entrypoint = prev.callPackage pkgs/cicero/entrypoint.nix {};
          cicero-evaluator-nix = prev.callPackage pkgs/cicero/evaluators/nix {
            inherit (nix2container.packages.${prev.system}) skopeo-nix2container;
          };
          webhook-trigger = prev.callPackage pkgs/trigger {};
          cicero-api = (final.extend haskell-nix.overlay).callPackage pkgs/cicero-api {
            inherit supportedSystems;
            src = ./.;
          };
          inherit (final.cicero-api) cicero-cli;

          # a coreutils package that also provides /usr/bin/env
          coreutils-env = prev.coreutils.overrideAttrs (oldAttrs: {
            postInstall = ''
              ${oldAttrs.postInstall}
              mkdir -p $out/usr/bin
              ln -s $out/bin/env $out/usr/bin/env
            '';
          });
        }
        // nixpkgs.lib.mapAttrs'
        (k: nixpkgs.lib.nameValuePair "cicero-evaluator-nix-run-${k}")
        (import pkgs/cicero/evaluators/nix/runners.nix prev);

      nixosModules.cicero = import nixos/modules/cicero.nix;
      nixosModule = self.nixosModules.cicero;

      nixosConfigurations = {
        vm = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {inherit self;};
          modules = [
            nixos/configs/vm.nix
            nixos-shell.nixosModules.nixos-shell
          ];
        };

        dev = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {inherit self;};
          modules = [nixos/configs/dev.nix];
        };

        cicero = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {inherit self;};
          modules = [nixos/configs/cicero.nix];
        };
      };

      lib = import ./lib.nix self;
    };
}
