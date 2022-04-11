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
    nix.url = "github:NixOS/nix/f22b9e72f51f97f8f2d334748d3e97123940a146";
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
    ...
  }: let
    supportedSystems = ["x86_64-linux"];
  in
    utils.lib.eachSystem supportedSystems
    (system: let
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
          dev-cicero-transformer = let
            post-build-hook = ''
              #! /bin/dash
              set -euf
              export IFS=' '
              echo 'Uploading to cache: '"$OUT_PATHS"
              exec nix copy --to 'http://127.0.0.1:7745/cache' $OUT_PATHS
            '';

            filter = ''
              .job[]?.datacenters |= . + ["dc1"] |
              .job[]?.group[]?.restart.attempts = 0 |
              .job[]?.group[]?.task[]?.vault.policies |= . + ["cicero"] |
              .job[]?.group[]?.task[]? |= if .config?.nixos then . else (
                .env |= . + {
                  CICERO_WEB_URL: "http://127.0.0.1:8080",
                  CICERO_API_URL: "http://127.0.0.1:8080",
                  NIX_CONFIG: (
                    "substituters = http://127.0.0.1:7745?compression=none\n" +
                    "extra-trusted-public-keys = spongix:yNfB2+pMSmrjNyMRWob1oEs4ihPnVKPkECWiDxv1MNI=\n" +
                    "post-build-hook = /local/post-build-hook\n" +
                    .NIX_CONFIG
                  ),
                } |
                .template |= . + [{
                  destination: "local/post-build-hook",
                  perms: "544",
                  data: env.postBuildHook,
                }] |
                .config.packages |= . + ["github:NixOS/nixpkgs/${nixpkgs.rev}#dash"]
              ) end
            '';
          in
            prev.writers.writeDashBin "dev-cicero-transformer" ''
              export postBuildHook=${prev.lib.escapeShellArg post-build-hook}
              jq ${prev.lib.escapeShellArg filter}
            '';
        })
        self.overlay
      ]);
      selfPkgs = self.packages.${system};
    in {
      packages = self.overlay (pkgs // selfPkgs) pkgs;
      defaultPackage = selfPkgs.cicero;
      hydraJobs = selfPkgs;
      devShell = pkgs.devshell.fromTOML ./devshell.toml;
      devShells.cicero-api = selfPkgs.cicero-api.project.shell;
      inherit (selfPkgs.cicero-api) apps;
    })
    // {
      overlay = final: prev:
        {
          cicero = prev.callPackage pkgs/cicero {flake = self;};
          cicero-entrypoint = prev.callPackage pkgs/cicero/entrypoint.nix {};
          cicero-evaluator-nix = prev.callPackage pkgs/cicero/evaluators/nix {flake = self;};
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

      ciceroActions =
        self.lib.callActionsWithExtraArgs
        rec {
          inherit (self.lib) std;
          inherit (nixpkgs) lib;
          actionLib = import ./action-lib.nix {inherit std lib;};
          nixpkgsRev = nixpkgs.rev;
        }
        ./actions;
    };
}
