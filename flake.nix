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
          version = "2021.10.26.001";
          vendorSha256 = "sha256-l/3rsRwop6DcBwZX5eE87ReuDl86Yp5A/j8QbDU1jgk=";

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

        run-script = (prev.writers.writeBashBin "run-script" ''
          set -exuo pipefail

          export PATH="$PATH:${prev.git}/bin:${prev.nixUnstable}/bin"
          export SSL_CERT_FILE="${prev.cacert}/etc/ssl/certs/ca-bundle.crt"

          ${prev.coreutils}/bin/mkdir -p /etc
          echo 'nixbld:x:30000:nixbld1' > /etc/group
          echo 'nixbld1:x:30001:30000:Nix build user 1:/var/empty:${prev.shadow}/bin/nologin' > /etc/passwd

          nix-store --load-db < /registration

          nix build -f ${./script.nix} \
              --experimental-features 'nix-command flakes' \
              --out-link script \
              --argstr language "$1" \
              --argstr script "$2"

          exec ./script
        '') // {
          requiredSystemFeatures = [ "recursive-nix" ];
        };
      };

      packages = { cicero, liftbridge, liftbridge-cli, bash, coreutils, gocritic
        , run-script }@pkgs:
        pkgs // {
          lib = nixpkgs.lib;
          defaultPackage = cicero;
        };

      hydraJobs = { cicero }@pkgs: pkgs;

      devShell = { devshell }: devshell.fromTOML ./devshell.toml;
    };
}
