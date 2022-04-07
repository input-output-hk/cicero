{
  self,
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    self.inputs.spongix.nixosModules.spongix
    ./dev.nix
  ];

  nix-driver-nomad.enable = false;

  nixpkgs.overlays = [
    self.inputs.spongix.overlay

    # TODO nix 2.8 fails this command:
    # nix profile install --profile ./x github:NixOS/nix#nix github:NixOS/nixpkgs#cacert
    self.inputs.nix.overlay
  ];

  nixos-shell.mounts = {
    cache = "none";
    extraMounts = {
      "/mnt/spongix" = rec {
        target = "${builtins.getEnv "PWD"}/.spongix";
        cache = "none";
      };

      # We are only interested in /etc/nix/netrc but
      # we can only mount directories, not single files.
      # See `systemd.tmpfiles.rules` below.
      "/etc/nix/host".target = /etc/nix;
    };
  };

  nix.extraOptions = ''
    netrc-file = /etc/nix/netrc
  '';

  virtualisation = {
    forwardPorts = [
      {
        # cicero
        from = "host";
        host.port = 8080;
        guest.port = 8080;
      }
      {
        # nomad
        from = "host";
        host.port = 4646;
        guest.port = 4646;
      }
      {
        from = "host";
        host.port = 8200;
        guest.port = lib.toInt (lib.last (lib.splitString ":" config.services.vault.address));
      }
      {
        from = "host";
        host.port = 8428;
        guest.port = lib.toInt (lib.last (lib.splitString ":" config.services.victoriametrics.listenAddress));
      }
      {
        from = "host";
        host.port = 3100;
        guest.port = config.services.loki.configuration.server.http_listen_port;
      }
      {
        from = "host";
        host.port = 7745;
        guest.port = config.services.spongix.port;
      }
      {
        from = "host";
        host.port = 5432;
        guest.port = config.services.postgresql.port;
      }
      {
        from = "guest";
        guest = {
          address = "10.0.2.100";
          port = 22;
        };
        host.port = 2255;
      }
    ];

    cores = 2;
    memorySize = 1024 * 4;
    diskSize = 1024 * 4; # nix builds need more inodes

    useNixStoreImage = true;
    writableStoreUseTmpfs = false;
  };

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
    exec ${self.outputs.devShell.${pkgs.system}}/entrypoint
  '';

  services = {
    nomad = {
      enable = true;
      enableDocker = false;
      dropPrivileges = false;

      extraSettingsPlugins = [
        self.inputs.driver.defaultPackage.${pkgs.system}
      ];
      extraPackages = with pkgs; [
        config.nix.package
        openssh
        gitMinimal
      ];

      settings = {
        log_level = "TRACE";

        plugin.nix_driver = {};

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
      };
    };

    vault = {
      enable = true;
      address = "0.0.0.0:8200";
      package = pkgs.vault-bin;
    };

    spongix = {
      enable = true;
      cacheDir = "/mnt/spongix";
      substituters = ["https://cache.nixos.org"];
      trustedPublicKeys = ["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="];
      secretKeyFiles."spongix.sec" = builtins.toFile "spongix.sec" "spongix:J5wXSq2iirA2sksFzfsV1fXoNQZFKh4QUOizy6b46sHI18Hb6kxKauM3IxFahvWgSziKE+dUo+QQJaIPG/Uw0g==";
    };
  };

  systemd.services = {
    create-netrc = rec {
      serviceConfig.Type = "oneshot";
      wantedBy = ["multi-user.target"];
      before = wantedBy;
      script = ''
        set -xuo pipefail

        rm -f /etc/nix/netrc

        if [[ -e /etc/nix/host/netrc ]]; then
          cat /etc/nix/host/netrc >> /etc/nix/netrc
        fi

        if [[ -f ${lib.escapeShellArg "${builtins.getEnv "HOME"}/.netrc"} ]]; then
          # ignore failure in case ~/.netrc is a link to /etc/nix/netrc
          cat "${builtins.getEnv "HOME"}/.netrc" >> /etc/nix/netrc || :
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

    nomad-follower = rec {
      description = "Nomad Follower";

      wantedBy = ["nomad.service"];
      bindsTo = wantedBy;
      after = bindsTo;

      serviceConfig = rec {
        ExecStart = "${self.inputs.follower.defaultPackage.${pkgs.system}}/bin/nomad-follower";

        Restart = "on-failure";
        RestartSec = 5;

        StateDirectory = "nomad-follower";
        WorkingDirectory = "/var/lib/${StateDirectory}";
      };
    };

    # for file permissions in mounted directory from host
    spongix.serviceConfig = builtins.mapAttrs (k: lib.mkForce) {
      User = "root";
      Group = "root";
      DynamicUser = false;
    };
  };
}