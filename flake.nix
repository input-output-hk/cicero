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

        cicero = prev.buildGoModule rec {
          pname = "cicero";
          version = "2021.10.11.001";
          vendorSha256 = "sha256-2qcMea6MvA/hhIQLt0mwiOk/KW7KiwHCT7l5FxqW6S0=";

          src =
            inputs.inclusive.lib.inclusive ./. [ ./go.mod ./go.sum ./main.go ];

          ldflags = [
            "-s"
            "-w"
            "-X main.buildVersion=${version}"
            "-X main.buildCommit=${self.rev or "dirty"}"
          ];
        };
      };

      packages = { cicero, liftbridge-cli, bash, coreutils }@pkgs:
        pkgs // {
          lib = nixpkgs.lib;
          defaultPackage = cicero;
        };

      hydraJobs = { cicero }@pkgs: pkgs;

      devShell = { devshell, go, goimports, gopls, gocode, gcc, dbmate }:
        devshell.fromTOML ./devshell.toml;
    };
}
