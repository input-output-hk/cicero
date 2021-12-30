{
  description = "Flake for the Cicero framework";

  inputs = {
    devshell.url = "github:numtide/devshell";
    inclusive.url = "github:input-output-hk/nix-inclusive";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-os.url = "github:manveru/nixpkgs/use-atomic-bind-mounts";
    utils.url = "github:kreisys/flake-utils";
    driver.url = "github:input-output-hk/nomad-driver-nix";
    follower.url = "github:input-output-hk/nomad-follower";
    data-merge.url = "github:divnix/data-merge";
  };

  outputs =
    { self, nixpkgs, nixpkgs-os, utils, devshell, driver, follower, ... }:
    utils.lib.simpleFlake {
      systems = [ "x86_64-linux" ];
      inherit nixpkgs;

      preOverlays = [ devshell.overlay ];

      overlay = final: prev:
        {
          cicero = prev.callPackage ./pkgs/cicero { flake = self; };
          cicero-std = prev.callPackage ./pkgs/cicero/std { };
          cicero-evaluator-nix =
            prev.callPackage ./pkgs/cicero/evaluators/nix { flake = self; };
          liftbridge = prev.callPackage ./pkgs/liftbridge.nix { };
          liftbridge-cli = prev.callPackage ./pkgs/liftbridge-cli.nix { };
          go = prev.go_1_17;
          gouml = prev.callPackage ./pkgs/gouml.nix { };
          gocritic = prev.callPackage ./pkgs/gocritic.nix { };
          webhook-trigger = prev.callPackage ./pkgs/trigger { };
          nomad-follower = follower.defaultPackage."${prev.system}";

          inherit (driver.legacyPackages.x86_64-linux) nomad-driver-nix;

          cicero-entrypoint =
            final.callPackage ./pkgs/cicero/entrypoint.nix { };

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

      packages = { cicero, cicero-std, cicero-evaluator-nix, cicero-entrypoint
        , liftbridge, liftbridge-cli, gocritic, go, webhook-trigger, nomad-dev
        , nomad-follower, run-bash, run-python, run-perl, run-js }@pkgs:
        pkgs // {
          inherit (nixpkgs) lib;
          defaultPackage = cicero;
        };

      extraOutputs.nixosConfigurations = {
        dev = nixpkgs-os.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [
            ({ pkgs, config, lib, ... }: {
              imports = [
                driver.nixosModules.nix-driver-nomad
                (nixpkgs-os + /nixos/modules/misc/version.nix)
                (nixpkgs-os + /nixos/modules/profiles/headless.nix)
                (nixpkgs-os + /nixos/modules/profiles/minimal.nix)
              ];

              nixpkgs.overlays = [ self.overlay ];
              networking.hostName = lib.mkDefault "dev";

              # re-enable TTY disabled by minimal profile for `machinectl shell`
              systemd.services."getty@tty1".enable = lib.mkForce true;

              systemd.services.liftbridge = {
                wantedBy = [ "multi-user.target" ];
                after = [ "network.target" ];

                serviceConfig = let
                  cfg = builtins.toFile "liftbridge.yaml" (builtins.toJSON {
                    listen = "0.0.0.0:9292";
                    host = "127.0.0.1";
                    port = "9292";
                    data.dir = "/local/server";
                    activity.stream.enabled = true;
                    logging = {
                      level = "debug";
                      raft = true;
                      nats = true;
                      recovery = true;
                    };
                    nats = {
                      embedded = true;
                      servers = [ ];
                    };
                    streams = {
                      retention.max = {
                        age = "24h";
                        messages = 1000;
                      };
                      compact.enabled = true;
                    };
                    clustering = {
                      server.id = "voter";
                      raft.bootstrap.seed = true;
                      replica.max.lag.time = "20s";
                    };
                  });
                in {
                  ExecStart =
                    "${pkgs.liftbridge}/bin/liftbridge --config ${cfg}";
                  Restart = "on-failure";
                  RestartSec = "5s";
                };
              };

              services.loki = {
                enable = true;
                configuration = {
                  auth_enabled = false;

                  ingester = {
                    chunk_idle_period = "5m";
                    chunk_retain_period = "30s";
                    lifecycler = {
                      address = "127.0.0.1";
                      final_sleep = "0s";
                      ring = {
                        kvstore = { store = "inmemory"; };
                        replication_factor = 1;
                      };
                    };
                  };

                  limits_config = {
                    enforce_metric_name = false;
                    reject_old_samples = true;
                    reject_old_samples_max_age = "168h";
                    ingestion_rate_mb = 160;
                    ingestion_burst_size_mb = 160;
                  };

                  schema_config = {
                    configs = [{
                      from = "2020-05-15";
                      index = {
                        period = "168h";
                        prefix = "index_";
                      };
                      object_store = "filesystem";
                      schema = "v11";
                      store = "boltdb";
                    }];
                  };

                  server = { http_listen_port = 3100; };

                  storage_config = {
                    boltdb = { directory = "/var/lib/loki/index"; };
                    filesystem = { directory = "/var/lib/loki/chunks"; };
                  };
                };
              };

              services.postgresql = {
                enable = true;
                enableTCPIP = true;
                package = pkgs.postgresql_12;

                settings = {
                  log_statement = "all";
                  log_destination = lib.mkForce "syslog";
                };

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
                  exec cicero start
                '';
              };
            })
          ];
        };
      };

      extraOutputs.lib = import ./lib.nix self;

      extraOutputs.ciceroActions = self.lib.callActionsWithExtraArgs rec {
        inherit (self.outputs.lib) std;
        inherit (self.inputs.nixpkgs) lib;
        actionLib = import ./action-lib.nix { inherit std lib; };
        nixpkgsRev = self.inputs.nixpkgs.rev;
      } ./actions;

      hydraJobs = { cicero }@pkgs: pkgs;

      devShell = { devshell }: devshell.fromTOML ./devshell.toml;
    };
}
