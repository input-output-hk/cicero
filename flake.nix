{
  description = "Flake for the Cicero framework";

  nixConfig = {
    extra-substituters = "https://cache.iog.io";
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  };

  inputs = {
    devshell.url = "github:numtide/devshell";
    nixos-shell = {
      url = "github:Mic92/nixos-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    inclusive.url = "github:input-output-hk/nix-inclusive";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    alejandra = {
      url = "github:kamadorueda/alejandra";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils.url = "github:numtide/flake-utils";
    nomad = {
      url = "github:input-output-hk/nomad/release/1.4.3";
      inputs.utils.follows = "utils";
    };
    follower = {
      url = "github:input-output-hk/nomad-follower";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    data-merge.url = "github:divnix/data-merge";
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs = {
        flake-utils.follows = "utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    spongix = {
      url = "github:input-output-hk/spongix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        inclusive.follows = "inclusive";
      };
    };
    nix2container.follows = "tullia/nix2container";
    tullia = {
      url = "github:input-output-hk/tullia";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    devshell,
    nixos-shell,
    nomad,
    follower,
    poetry2nix,
    spongix,
    alejandra,
    nix2container,
    tullia,
    ...
  }: let
    supportedSystems = ["x86_64-linux" "x86_64-darwin"];
  in
    utils.lib.eachSystem supportedSystems
    (
      system: let
        pkgs = nixpkgs.legacyPackages.${system}.extend (nixpkgs.lib.composeManyExtensions [
          devshell.overlay
          poetry2nix.overlay
          follower.overlay
          spongix.overlay
          (final: prev: {
            nixos-shell = nixos-shell.defaultPackage.${prev.system};
            alejandra = alejandra.defaultPackage.${prev.system};
            go = prev.go_1_18;
            go-mockery = prev.callPackage pkgs/mockery.nix {};
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
              prev.writers.writeDashBin "dev-cicero-transformer" ''
                jq --compact-output \
                  --argjson args ${nixpkgs.lib.escapeShellArg (builtins.toJSON args)} \
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
        }
        // tullia.fromSimple system (import ./tullia.nix)
    )
    // {
      overlay = final: prev:
        {
          cicero = prev.callPackage pkgs/cicero {flake = self;};
          cicero-entrypoint = prev.callPackage pkgs/cicero/entrypoint.nix {};
          cicero-evaluator-nix = prev.callPackage pkgs/cicero/evaluators/nix {
            inherit (nix2container.packages.${prev.system}) skopeo-nix2container;
          };
          webhook-trigger = prev.callPackage pkgs/trigger {flake = self;};

          # a coreutils package that also provides /usr/bin/env
          coreutils-env = prev.coreutils.overrideAttrs (oldAttrs: {
            postInstall = ''
              ${oldAttrs.postInstall}
              mkdir -p $out/usr/bin
              ln -s $out/bin/env $out/usr/bin/env
            '';
          });

          nomad-driver-podman = prev.callPackage pkgs/nomad-driver-podman.nix {};
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
            {
              nixpkgs.overlays = [
                (_: prev: {
                  inherit (nomad.packages.${prev.system}) nomad;
                  inherit (self.outputs.packages.${prev.system}) nomad-driver-podman;
                })
              ];
            }
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
