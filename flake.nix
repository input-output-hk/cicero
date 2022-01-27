{
  description = "Flake for the Cicero framework";

  inputs = {
    devshell.url = "github:numtide/devshell";
    inclusive.url = "github:input-output-hk/nix-inclusive";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
  };

  outputs = { self, nixpkgs, utils, devshell, driver, follower, poetry2nix, ... }:
    utils.lib.eachSystem [ "x86_64-linux" ] (system: let
      pkgs = nixpkgs.legacyPackages.${system}.extend (nixpkgs.lib.composeManyExtensions [
        devshell.overlay
        poetry2nix.overlay
        follower.overlay
        (final: prev: {
          go = prev.go_1_17;
          gouml = final.callPackage pkgs/gouml.nix { };
          gocritic = final.callPackage pkgs/gocritic.nix { };
          schemathesis = final.callPackage pkgs/schemathesis.nix { };
          nomad-dev = pkgs.writeShellScriptBin "nomad-dev" ''
            set -exuo pipefail

            # Preserve PATH for systems that
            # don't have nix in their root's PATH,
            # like conventional linux distros
            # with a standalone nix install.
            sudo --preserve-env=PATH \
              ${pkgs.nomad}/bin/nomad agent -dev \
              -plugin-dir ${driver.defaultPackage.${system}}/bin \
              -config ${pkgs.writeText "nomad.hcl" (builtins.toJSON {
                log_level = "TRACE";
                plugin.nix_driver = {};
                client.cni_path = "${pkgs.cni-plugins}/bin";
              })}
          '';
        })
        self.overlay
      ]);
    in {
      packages = self.overlay (pkgs // self.packages.${system}) pkgs;
      defaultPackage = self.packages.${system}.cicero;
      hydraJobs = self.packages.${system};
      devShell = pkgs.devshell.fromTOML ./devshell.toml;
    }) // {
      overlay = final: prev: {
        cicero = prev.callPackage pkgs/cicero { flake = self; };
        cicero-entrypoint = prev.callPackage pkgs/cicero/entrypoint.nix { };
        cicero-evaluator-nix = prev.callPackage pkgs/cicero/evaluators/nix { flake = self; };
        webhook-trigger = prev.callPackage pkgs/trigger { };
      } // nixpkgs.lib.mapAttrs'
        (k: nixpkgs.lib.nameValuePair "cicero-evaluator-nix-run-${k}")
        (import pkgs/cicero/evaluators/nix/runners.nix prev);

      nixosModules.cicero = import nixos/modules/cicero.nix;
      nixosModule = self.nixosModules.cicero;

      nixosConfigurations = {
        dev = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [ nixos/configs/dev.nix ];
        };

        cicero = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [ nixos/configs/cicero.nix ];
        };
      };

      lib = import ./lib.nix self;

      ciceroActions = self.lib.callActionsWithExtraArgs
        rec {
          inherit (self.lib) std;
          inherit (self.inputs.nixpkgs) lib;
          actionLib = import ./action-lib.nix { inherit std lib; };
          nixpkgsRev = self.inputs.nixpkgs.rev;
        } ./actions;
    };
}
