{
  description = "Flake for the Cicero framework";

  inputs = {
    devshell.url = "github:numtide/devshell";
    inclusive.url = "github:input-output-hk/nix-inclusive";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:kreisys/flake-utils";
  };

  outputs = { self, nixpkgs, utils, devshell, ... }@inputs:
    utils.lib.simpleFlake {
      systems = [ "x86_64-linux" ];
      inherit nixpkgs;

      preOverlays = [ devshell.overlay ];

      overlay = final: prev: {
        liftbridge = prev.callPackage ./pkgs/liftbridge.nix { };

        liftbridge-cli = prev.callPackage ./pkgs/liftbridge-cli.nix { };

        gouml = prev.callPackage ./pkgs/gouml.nix { };

        gocritic = prev.callPackage ./pkgs/gocritic.nix { };

        cicero = prev.buildGoModule rec {
          pname = "cicero";
          version = "2021.10.19.001";
          vendorSha256 = "sha256-F9GgiH55rkiYLYi4K1A0nNxkdzQs/29Ncy5Vp3mY2PY=";

          src = inputs.inclusive.lib.inclusive ./. [
            ./src
            ./db
            ./go.mod
            ./go.sum
            ./lib.nix
            ./main.go
          ];

          ldflags = [
            "-s"
            "-w"
            "-X main.buildVersion=${version}"
            "-X main.buildCommit=${self.rev or "dirty"}"
          ];
        };
      };

      packages = { cicero, liftbridge-cli, bash, coreutils, gocritic }@pkgs:
        pkgs // {
          lib = nixpkgs.lib;
          defaultPackage = cicero;
        };

      hydraJobs = { cicero }@pkgs: pkgs;

      devShell = { devshell }: devshell.fromTOML ./devshell.toml;
    };
}
