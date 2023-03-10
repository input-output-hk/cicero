parts @ {
  inputs,
  getSystem,
  lib,
  ...
}: {
  flake.nixosConfigurations.vm = lib.makeOverridable inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      ({
        config,
        lib,
        pkgs,
        ...
      }: {
        imports = [
          parts.config.flake.nixosModules.default

          inputs.nixos-shell.nixosModules.nixos-shell
          inputs.spongix.nixosModules.spongix
          inputs.follower.nixosModules.nomad-follower

          # TODO use nixpkgs nomad module again once merged
          # https://github.com/NixOS/nixpkgs/pull/147670
          "${builtins.getFlake github:GTrunSec/nixpkgs/26ad08f8c4e718b3a5a5e7a35bfc1745c4245eb9}/nixos/modules/services/networking/nomad.nix"
        ];

        disabledModules = ["services/networking/nomad.nix"];

        nixpkgs.overlays = [
          inputs.spongix.overlay
          inputs.follower.overlay
          (_: prev: {
            inherit (parts.inputs.nomad.packages.${prev.system}) nomad;
            inherit ((getSystem prev.system).packages) cicero nomad-driver-podman;
          })
        ];

        nixos-shell.mounts = {
          cache = "none";
          extraMounts = let
            pwd = builtins.getEnv "PWD";
          in {
            ${pwd}.target = pwd;

            "/mnt/spongix".target = "${pwd}/.spongix";

            # We are only interested in /etc/nix/netrc but
            # we can only mount directories, not single files.
            # See `systemd.services.create-netrc` below.
            "/etc/nix/host".target = /etc/nix;

            ${config.services.dockerRegistry.storagePath}.target = "${pwd}/.docker-registry";
          };
        };

        nix = {
          settings = {
            experimental-features = ["nix-command" "flakes"];
            substituters = ["http://127.0.0.1:${toString config.services.spongix.port}"];
            trusted-public-keys = ["spongix:yNfB2+pMSmrjNyMRWob1oEs4ihPnVKPkECWiDxv1MNI="];
            require-sigs = false; # TODO remove once spongix signs with own key again
            netrc-file = /etc/nix/netrc;
            post-build-hook = pkgs.writers.writeBash "post-build-hook" ''
              set -euf
              export IFS=' '
              if [[ -n "$OUT_PATHS" ]]; then
                echo 'Uploading to cache: '"$OUT_PATHS"
                exec nix copy --to 'http://127.0.0.1:17745?compression=none' $OUT_PATHS
              fi
            '';
          };
        };

        virtualisation = {
          forwardPorts =
            map (
              # set all ports to be the same on host and guest
              # so that we can run cicero and devshell programs wherever
              port: {
                from = "host";
                host = {inherit port;};
                guest = {inherit port;};
              }
            ) [
              18080 # cicero
              config.services.nomad.settings.ports.http
              (lib.toInt (lib.last (lib.splitString ":" config.services.vault.address)))
              (lib.toInt (lib.last (lib.splitString ":" config.services.victoriametrics.listenAddress)))
              config.services.loki.configuration.server.http_listen_port
              config.services.spongix.port
              config.services.postgresql.port
              config.services.dockerRegistry.port
            ];

          cores =
            if builtins.pathExists /proc/cpuinfo
            then let
              cpuinfo = lib.fileContents /proc/cpuinfo;
              num = builtins.length (lib.splitString "processor\t: " cpuinfo) - 1;
            in
              builtins.floor (num / 1.5 + 1)
            else 8;
          memorySize =
            if builtins.pathExists /proc/meminfo
            then let
              meminfo = lib.fileContents /proc/meminfo;
              kb = lib.toInt (lib.last (
                builtins.match "MemTotal:[[:space:]]+([[:digit:]]+) kB\n.*" meminfo
              ));
            in
              builtins.floor (kb / 1.5 / 1024)
            else 1024 * 4;
          diskSize = 1024 * 20; # nix builds need more inodes

          useNixStoreImage = true;
          writableStore = true;
          writableStoreUseTmpfs = false;

          podman = {
            enable = true;
            dockerCompat = true;
            dockerSocket.enable = true;
            defaultNetwork.dnsname.enable = true;
          };

          containers.registries.insecure = [
            "127.0.0.1:${toString config.services.dockerRegistry.port}"
          ];
        };

        networking.firewall.enable = false;

        environment = {
          systemPackages = with pkgs; [
            vault
            gitMinimal # for go-getter
          ];

          sessionVariables = {
            inherit (config.systemd.services.vault.environment) VAULT_ADDR VAULT_TOKEN;
          };
        };

        programs.bash.loginShellInit = lib.mkAfter ''
          PRJ_ROOT=${lib.escapeShellArg (builtins.getEnv "PWD")} \
          exec ${parts.config.flake.devShells.${pkgs.system}.default}/entrypoint
        '';

        services = {
          cicero.enable = true;

          nomad = {
            enable = true;
            enableDocker = false;
            dropPrivileges = false;

            extraSettingsPlugins = [
              pkgs.nomad-driver-podman
            ];
            extraPackages = with pkgs; [
              config.nix.package
              openssh
              gitMinimal
            ];

            settings = {
              log_level = "TRACE";

              plugin.podman = {};

              server = {
                enabled = true;
                bootstrap_expect = 1;
              };

              client = {
                enabled = true;
                cni_path = "${pkgs.cni-plugins}/bin";
              };

              vault = let
                inherit (config.systemd.services.vault.environment) VAULT_ADDR VAULT_TOKEN;
              in {
                enabled = true;
                address = VAULT_ADDR;
                token = VAULT_TOKEN;
              };

              ports.http = 14646;
            };
          };

          nomad-follower = {
            enable = true;
            nomadAddr = "http://127.0.0.1:${toString config.services.nomad.settings.ports.http}";
            nomadTokenFile = "";
            lokiUrl = "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}";
            prometheusUrl = let
              addr = config.services.victoriametrics.listenAddress;
            in
              assert lib.hasPrefix ":" addr; "http://127.0.0.1${addr}/api/v1/write";
          };

          vault = {
            enable = true;
            address = "0.0.0.0:18200";
            package = pkgs.vault-bin;
          };

          spongix = {
            enable = true;
            port = 17745;
            cacheDir = "/mnt/spongix";
            substituters = ["https://cache.nixos.org"];
            trustedPublicKeys = ["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="];
            secretKeyFiles."spongix.sec" = builtins.toFile "spongix.sec" "spongix:J5wXSq2iirA2sksFzfsV1fXoNQZFKh4QUOizy6b46sHI18Hb6kxKauM3IxFahvWgSziKE+dUo+QQJaIPG/Uw0g==";
          };

          postgresql = {
            enableTCPIP = true;
            port = 15432;

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

          dockerRegistry = {
            enable = true;
            enableDelete = true;
            enableGarbageCollect = true;
            listenAddress = "0.0.0.0";
            port = 15000;
          };

          victoriametrics = {
            enable = true;
            listenAddress = ":18428";
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

              server.http_listen_port = 13100;

              storage_config = {
                boltdb.directory = "/var/lib/loki/index";
                filesystem.directory = "/var/lib/loki/chunks";
              };
            };
          };
        };

        systemd = {
          enableUnifiedCgroupHierarchy = false;

          services = {
            # for development we want to run cicero outside the container or start it manually inside
            cicero.enable = lib.mkForce false;

            create-netrc = rec {
              serviceConfig.Type = "oneshot";
              wantedBy = ["multi-user.target"];
              before = wantedBy;
              script = ''
                set -xuo pipefail

                rm -f /etc/nix/netrc

                if [[ -e /etc/nix/host/netrc ]]; then
                  # ignore failure in case the host does not have /etc/nix/netrc
                  cat /etc/nix/host/netrc >> /etc/nix/netrc || :
                fi

                if [[ -f ${lib.escapeShellArg (builtins.getEnv "HOME")}/.netrc ]]; then
                  # ignore failure in case ~/.netrc is a link to /etc/nix/netrc
                  cat ${lib.escapeShellArg (builtins.getEnv "HOME")}/.netrc >> /etc/nix/netrc || :
                fi

                if [[ -n ${lib.escapeShellArg (builtins.getEnv "NETRC")} ]]; then
                  printf "%s\n" ${lib.escapeShellArg (builtins.getEnv "NETRC")} >> /etc/nix/netrc
                fi
              '';
            };

            vault = {
              after = ["create-netrc.service"];

              path = with pkgs; [vault netcat];

              environment = rec {
                VAULT_DEV_ROOT_TOKEN_ID = "root";
                VAULT_DEV_LISTEN_ADDRESS = config.services.vault.address;

                VAULT_ADDR = "http://${VAULT_DEV_LISTEN_ADDRESS}";
                VAULT_TOKEN = VAULT_DEV_ROOT_TOKEN_ID;

                REGISTRY_AUTH_FILE = "${builtins.getEnv "HOME"}/.docker/config.json";
              };

              serviceConfig = {
                WorkingDirectory = "~";
                ExecStart = lib.mkForce "${config.services.vault.package}/bin/vault server -dev -dev-no-store-token";
                ExecStartPost = pkgs.writers.writeBash "init" ''
                  set -xeuo pipefail

                  while ! nc -z ${lib.escapeShellArgs (
                    lib.splitString ":" config.services.vault.address
                  )} &> /dev/null; do
                    >&2 echo 'Waiting for Vault…'
                    sleep 1
                  done

                  cat <<EOF | vault policy write cicero -
                  path "auth/token/lookup" {
                    capabilities = ["update"]
                  }
                  path "auth/token/lookup-self" {
                    capabilities = ["read"]
                  }
                  path "auth/token/renew-self" {
                    capabilities = ["update"]
                  }
                  path "kv/data/cicero/*" {
                    capabilities = ["read", "list"]
                  }
                  path "kv/metadata/cicero/*" {
                    capabilities = ["read", "list"]
                  }
                  path "nomad/creds/cicero" {
                    capabilities = ["read", "update"]
                  }
                  EOF

                  vault secrets enable -version=2 kv

                  vault kv put kv/cicero/github webhooks=correct-horse-battery-staple

                  if [[ -e /etc/nix/netrc ]]; then
                    token=$(
                      grep -E 'machine[[:space:]]+github.com' /etc/nix/netrc -A 2 \
                      | grep password \
                      | cut -d ' ' -f 2
                    )
                    if [[ -n "$token" ]]; then
                      vault kv patch kv/cicero/github token="$token"
                    fi
                  fi
                '';
              };
            };

            # for file permissions in mounted directory from host
            spongix.serviceConfig = builtins.mapAttrs (k: lib.mkForce) {
              User = "root";
              Group = "root";
              DynamicUser = false;
            };

            # for file permissions in mounted directory from host
            docker-registry.serviceConfig = builtins.mapAttrs (k: lib.mkForce) {
              User = "root";
              Group = "root";
            };
          };
        };
      })
    ];
  };
}
