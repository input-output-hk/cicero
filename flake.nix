{
  description = "Flake for the Cicero framework";

  inputs = {
    devshell.url = "github:numtide/devshell";
    inclusive.url = "github:input-output-hk/nix-inclusive";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-os.url = "github:manveru/nixpkgs/use-atomic-bind-mounts";
    utils.url = "github:kreisys/flake-utils";
    driver.url = "github:input-output-hk/nomad-driver-nix";
  };

  outputs = { self, nixpkgs, nixpkgs-os, utils, devshell, driver, ... }:
    utils.lib.simpleFlake {
      systems = [ "x86_64-linux" ];
      inherit nixpkgs;

      preOverlays = [ devshell.overlay ];

      overlay = final: prev:
        {
          cicero = prev.callPackage ./pkgs/cicero { flake = self; };
          cicero-evaluator-nix =
            prev.callPackage ./pkgs/cicero/evaluators/nix { flake = self; };
          liftbridge = prev.callPackage ./pkgs/liftbridge.nix { };
          liftbridge-cli = prev.callPackage ./pkgs/liftbridge-cli.nix { };
          gouml = prev.callPackage ./pkgs/gouml.nix { };
          gocritic = prev.callPackage ./pkgs/gocritic.nix { };
          wfs = prev.writeShellScriptBin "wfs" ''
            exec ${final.cicero-evaluator-nix}/bin/cicero-evaluator-nix "''${1:-}" ./workflows
          '';

          inherit (driver.legacyPackages.x86_64-linux) nomad-driver-nix;

          edgedb-cli = let
            src = builtins.fetchurl {
              url =
                "https://packages.edgedb.com/archive/linux-x86_64/edgedb-cli_1.0.0-rc.1_202109302039";
              sha256 =
                "sha256:1vljq8az2fcy8h4fwk840irhc47xn0hkngjfa2hsby3ryqdbp60b";
            };
          in final.runCommand "edgedb" { } ''
            mkdir -p $out/bin
            cp ${src} $out/bin/edgedb
            chmod +x $out/bin/edgedb
          '';

          nomad-dev = let
            cfg = builtins.toFile "nomad.hcl" ''
              log_level = "TRACE"
              plugin "nix_driver" {}
            '';
          in prev.writeShellScriptBin "nomad-dev" ''
            set -exuo pipefail

            sudo ${prev.nomad}/bin/nomad \
              agent \
              -dev \
              -config ${cfg} \
              -plugin-dir "${final.nomad-driver-nix}/bin"
          '';
        } // (import ./runners.nix final prev);

      packages = { cicero, cicero-evaluator-nix, liftbridge, liftbridge-cli
        , gocritic, edgedb-cli, nomad-dev, wfs, run-bash, run-python, run-perl
        , run-js }@pkgs:
        pkgs // {
          lib = nixpkgs.lib;
          defaultPackage = cicero;
        };

      extraOutputs.nixosConfigurations = {
        postgres = nixpkgs-os.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [
            ({ pkgs, config, lib, ... }: {
              nixpkgs.overlays = [ self.overlay ];
              system.build.closure = pkgs.buildPackages.closureInfo {
                rootPaths = [ config.system.build.toplevel ];
              };

              imports = [
                (nixpkgs-os + /nixos/modules/misc/version.nix)
                (nixpkgs-os + /nixos/modules/profiles/base.nix)
                (nixpkgs-os + /nixos/modules/profiles/headless.nix)
                (nixpkgs-os + /nixos/modules/profiles/minimal.nix)
                (nixpkgs-os + /nixos/modules/profiles/qemu-guest.nix)
              ];

              boot.isContainer = true;
              networking.useDHCP = false;
              networking.hostName = lib.mkDefault "postgres";

              boot.postBootCommands = ''
                # After booting, register the contents of the Nix store in the container in the Nix database in the tmpfs.
                ${config.nix.package.out}/bin/nix-store --load-db < /registration
                # nixos-rebuild also requires a "system" profile and an /etc/NIXOS tag.
                touch /etc/NIXOS
                ${config.nix.package.out}/bin/nix-env -p /nix/var/nix/profiles/system --set /run/current-system
              '';

              services.postgresql = {
                enable = true;
                enableTCPIP = true;
                package = pkgs.postgresql_12;

                authentication = ''
                  local all all trust
                  host all all 127.0.0.1/32 trust
                  host all all ::1/128 trust
                '';

                initialScript = pkgs.writeText "init.sql" ''
                  CREATE DATABASE cicero;
                  CREATE USER cicero;
                  GRANT ALL PRIVILEGES ON DATABASE cicero to cicero;
                  ALTER USER cicero WITH SUPERUSER;
                '';
              };
            })
          ];
        };

        cicero = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [
            (nixpkgs + /nixos/modules/misc/version.nix)
            (nixpkgs + /nixos/modules/profiles/base.nix)
            (nixpkgs + /nixos/modules/profiles/headless.nix)
            (nixpkgs + /nixos/modules/profiles/minimal.nix)
            (nixpkgs + /nixos/modules/profiles/qemu-guest.nix)
            ({ pkgs, config, lib, ... }: {
              # boot.isContainer = true;
              networking.useDHCP = false;
              networking.hostName = "cicero";
              nixpkgs.overlays = [ self.overlay ];

              nix = {
                package = pkgs.nixUnstable;
                systemFeatures = [ "recursive-nix" "nixos-test" ];
                extraOptions = ''
                  experimental-features = nix-command flakes ca-references recursive-nix
                '';
              };

              users.users = {
                nixos = {
                  isNormalUser = true;
                  extraGroups = [ "wheel" ];
                  initialHashedPassword = "";
                };

                root.initialHashedPassword = "";
              };

              security.sudo = {
                enable = lib.mkDefault true;
                wheelNeedsPassword = lib.mkForce false;
              };

              services.getty.autologinUser = "nixos";

              services.openssh = {
                enable = true;
                permitRootLogin = "yes";
              };

              boot.postBootCommands = ''
                # After booting, register the contents of the Nix store in the container in the Nix database in the tmpfs.
                ${config.nix.package.out}/bin/nix-store --load-db < /registration
                # nixos-rebuild also requires a "system" profile and an /etc/NIXOS tag.
                touch /etc/NIXOS
                ${config.nix.package.out}/bin/nix-env -p /nix/var/nix/profiles/system --set /run/current-system
              '';

              systemd.services.cicero = {
                wantedBy = [ "multi-user.target" ];
                after = [ "network.target" ];
                path = with pkgs; [ cicero ];
                script = ''
                  exec cicero all
                '';
              };
            })
          ];
        };
      };

      hydraJobs = { cicero }@pkgs: pkgs;

      devShell = { devshell }: devshell.fromTOML ./devshell.toml;
    };
}
