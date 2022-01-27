{ self, pkgs, config, lib, ... }:

{
  imports = [
    self.inputs.driver.nixosModules.nix-driver-nomad
    "${self.inputs.nixpkgs}/nixos/modules/misc/version.nix"
    "${self.inputs.nixpkgs}/nixos/modules/profiles/headless.nix"
    "${self.inputs.nixpkgs}/nixos/modules/profiles/minimal.nix"
  ];

  nixpkgs.overlays = [ self.overlay ];

  networking.hostName = lib.mkDefault "dev";

  # re-enable TTY disabled by minimal profile for `machinectl shell`
  systemd.services."getty@tty1".enable = lib.mkForce true;

  services = {
    loki = {
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
              kvstore.store = "inmemory";
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

        server.http_listen_port = 3100;

        storage_config = {
          boltdb.directory = "/var/lib/loki/index";
          filesystem.directory = "/var/lib/loki/chunks";
        };
      };
    };

    postgresql = {
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
        GRANT ALL PRIVILEGES ON DATABASE cicero TO cicero;
        ALTER USER cicero WITH SUPERUSER;

        CREATE ROLE cicero_api;
      '';
    };
  };
}
