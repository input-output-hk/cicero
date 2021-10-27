{
  description = "Flake for the Cicero framework";

  inputs = {
    devshell.url = "github:numtide/devshell";
    inclusive.url = "github:input-output-hk/nix-inclusive";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:kreisys/flake-utils";
  };

  outputs = { self, nixpkgs, utils, devshell, ... }:
    utils.lib.simpleFlake {
      systems = [ "x86_64-linux" ];
      inherit nixpkgs;

      preOverlays = [ devshell.overlay ];

      overlay = final: prev:
        {
          cicero = prev.callPackage ./pkgs/cicero.nix { flake = self; };
          liftbridge = prev.callPackage ./pkgs/liftbridge.nix {};
          liftbridge-cli = prev.callPackage ./pkgs/liftbridge-cli.nix {};
          gouml = prev.callPackage ./pkgs/gouml.nix {};
          gocritic = prev.callPackage ./pkgs/gocritic.nix {};
        } // (import ./runners.nix final prev);

      packages =
        { cicero
        , liftbridge
        , liftbridge-cli
        , gocritic
        , run-bash
        , run-python
        , run-perl
        , run-js
        } @ pkgs:
          pkgs // {
            lib = nixpkgs.lib;
            defaultPackage = cicero;
          };

      hydraJobs = { cicero } @ pkgs: pkgs;

      devShell = { devshell }: devshell.fromTOML ./devshell.toml;
    };
}
