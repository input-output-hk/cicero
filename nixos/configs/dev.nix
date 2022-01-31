{ lib, ... }:

{
  imports = [ ./cicero.nix ];

  # for development we want to run cicero outside the container
  systemd.services.cicero.enable = lib.mkForce false;

  services = {
    postgresql = {
      enableTCPIP = true;

      settings = {
        log_statement = "all";
        log_destination = lib.mkForce "syslog";
        shared_preload_libraries = "pg_stat_statements";
        "pg_stat_statements.track" = "all";
      };

      authentication = ''
        local all all trust
        host all all 0.0.0.0/0 trust
        host all all ::1/0 trust
      '';
    };

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
          configs = [
            {
              from = "2020-05-15";
              index = {
                period = "168h";
                prefix = "index_";
              };
              object_store = "filesystem";
              schema = "v11";
              store = "boltdb";
            }
          ];
        };

        server.http_listen_port = 3100;

        storage_config = {
          boltdb.directory = "/var/lib/loki/index";
          filesystem.directory = "/var/lib/loki/chunks";
        };
      };
    };

    victoriametrics.enable = true;
  };
}
