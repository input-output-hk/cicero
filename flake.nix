{
  nixConfig = {
    extra-substituters = "https://cache.iog.io";
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  };

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;
    nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    parts.url = github:hercules-ci/flake-parts;
    devshell = {
      url = github:numtide/devshell;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix.url = github:numtide/treefmt-nix;
    nixos-shell = {
      url = github:Mic92/nixos-shell;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    inclusive.url = github:input-output-hk/nix-inclusive;
    nomad = {
      url = github:input-output-hk/nomad/release/1.4.3;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    follower = {
      url = github:input-output-hk/nomad-follower;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    spongix = {
      # tests fail on latest main
      url = github:input-output-hk/spongix/a08bcc8f0bb63e372bf82abe5ad1f001e6b4d566;
      inputs = {
        nixpkgs.follows = "nixpkgs";
        inclusive.follows = "inclusive";
      };
    };
    nix2container.follows = "tullia/nix2container";
    tullia = {
      url = github:input-output-hk/tullia;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "x86_64-darwin"];

      imports = [
        ./devShells.nix
        ./treefmt.nix
        ./tullia.nix
        ./lib.nix
        ./overlay.nix
        packages/cicero
        packages/cicero/evaluators/nix
        packages/cicero/handbook.nix
        packages/trigger.nix
        packages/nomad-driver-podman.nix
        packages/go-critic.nix
        packages/treefmt-cue.nix
        nixos/modules
        nixos/configs/vm.nix
      ];

      perSystem = {inputs', ...}: {
        _module.args.pkgs = inputs'.nixpkgs.legacyPackages.extend (_: _: {
          inherit
            (inputs'.nixpkgs-unstable.legacyPackages)
            go-mockery # We need at least v2.17.0 for Go 1.20 support.
            golangci-lint # We need at least v1.51.0 for Go 1.20 support.
            ;
        });
      };
    };
}
