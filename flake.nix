{
  description = "Flake for the Cicero framework";

  inputs = {
    devshell.url = "github:numtide/devshell";
    inclusive.url = "github:input-output-hk/nix-inclusive";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    driver = {
      url = "github:input-output-hk/nomad-driver-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    follower = {
      url = "github:input-output-hk/nomad-follower";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    data-merge.url = "github:divnix/data-merge";
    poetry2nix = {
      url = "github:nix-community/poetry2nix/fetched-projectdir-test";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-cache-proxy = {
      url = "github:input-output-hk/nix-cache-proxy";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        inclusive.follows = "inclusive";
      };
    };
  };

  outputs = { self, nixpkgs, utils, devshell, driver, follower, poetry2nix, nix-cache-proxy, ... }:
    utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          nix-cache-proxy-key = {
            secret = "nix-cache-proxy:J5wXSq2iirA2sksFzfsV1fXoNQZFKh4QUOizy6b46sHI18Hb6kxKauM3IxFahvWgSziKE+dUo+QQJaIPG/Uw0g==";
            public = "nix-cache-proxy:yNfB2+pMSmrjNyMRWob1oEs4ihPnVKPkECWiDxv1MNI=";
          };

          pkgs = nixpkgs.legacyPackages.${system}.extend (nixpkgs.lib.composeManyExtensions [
            devshell.overlay
            poetry2nix.overlay
            follower.overlay
            nix-cache-proxy.overlay
            (final: prev: rec {
              go = prev.go_1_17;
              gouml = final.callPackage pkgs/gouml.nix { };
              gocritic = final.callPackage pkgs/gocritic.nix { };
              schemathesis = final.callPackage pkgs/schemathesis.nix { };
              dev-cluster = pkgs.writers.writeBashBin "dev-cluster" ''
                set -euo pipefail

                export PATH=${with prev; lib.makeBinPath [
                  netcat
                  nomad
                  nomad-follower
                  vault-bin
                  prev.nix-cache-proxy
                ]}:"$PATH"

                >&2 echo 'Please authorize sudo (type your password):'
                >&2 sudo echo 'Thanks! Starting…'

                # basically stolen from https://github.com/chrismytton/shoreman
                function log {
                  local index=$1
                  local name="$2"

                  local color=$((31 + (index % 7)))
                  while IFS= read; do
                    >&2 printf "\033[0;''${color}m%s |\033[0m " "$name"
                    printf "%s\n" "$REPLY"
                  done
                }

                function cleanup {
                  >&2 echo 'Stopping processes… (please provide password again if necessary)'
                  sudo kill $(jobs -p) 2> /dev/null || :

                  sleep 1
                  if [[ -n "$(jobs -rp)" ]]; then
                    >&2 echo 'There are survivors:'
                    jobs -l
                    >&2 echo 'Retrying…'
                    cleanup
                  fi
                }
                trap cleanup EXIT

                export VAULT_DEV_ROOT_TOKEN_ID=root
                vault server -dev |& log 0 vault &

                export VAULT_ADDR=http://127.0.0.1:8200
                export VAULT_TOKEN=$VAULT_DEV_ROOT_TOKEN_ID

                while ! nc -z 127.0.0.1 8200 &> /dev/null; do
                  echo 'Waiting for Vault…' |& log 0 vault
                  sleep 1
                done

                cat <<EOF | vault policy write cicero - |& log 0 vault
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

                vault secrets enable -version=2 kv |& log 0 vault

                vault kv put kv/cicero/github \
                  webhooks=correct-horse-battery-staple \
                |& log 0 vault

                read -rsp 'Optionally provide a GitHub personal access token: '
                if [[ -n "$REPLY" ]]; then
                  vault kv patch kv/cicero/github token="$REPLY" |& log 0 vault
                fi

                # Preserve PATH for systems that
                # don't have nix in their root's PATH,
                # like conventional linux distros
                # with a standalone nix install.
                # Also start Nomad by absolute path because
                # some systems do not find it through sudo.
                sudo --preserve-env=PATH,VAULT_TOKEN \
                  $(which nomad) agent -dev \
                  -plugin-dir ${driver.defaultPackage.${system}}/bin \
                  -config ${prev.writeText "nomad.hcl" (builtins.toJSON {
                    log_level = "TRACE";
                    plugin.nix_driver = {};
                    client.cni_path = "${prev.cni-plugins}/bin";
                    vault = {
                      enabled = true;
                      address = "http://127.0.0.1:8200";
                    };
                  })} \
                |& log 1 nomad &

                {
                  while ! nc -z 127.0.0.1 4646 &> /dev/null; do
                    echo 'Waiting for Nomad…'
                    sleep 1
                  done
                  # Start nomad-follower by absolute path because
                  # some systems do not find it through sudo.
                  sudo $(which nomad-follower)
                } |& log 2 follower &

                nix-cache-proxy \
                  --substituters 'https://cache.nixos.org' \
                  --secret-key-files ${builtins.toFile "nix-cache-proxy.sec" nix-cache-proxy-key.secret} \
                  --trusted-public-keys 'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' \
                  --listen :7745 \
                  --dir nix-cache-proxy \
                  |& log 3 nix-cache-proxy &

                wait
              '';
              dev-cicero-transformer = let
                post-build-hook = ''
                  #! /bin/bash
                  set -euf
                  export IFS=' '
                  echo 'Uploading to cache: '"$OUT_PATHS"
                  exec nix copy --to 'http://127.0.0.1:7745/cache' $OUT_PATHS
                '';

                filter = ''
                  .job[]?.datacenters |= . + ["dc1"] |
                  .job[]?.group[]?.restart.attempts = 0 |
                  .job[]?.group[]?.task[]?.env |= . + {
                      CICERO_WEB_URL: "http://127.0.0.1:8080",
                      NIX_CONFIG: (
                        "extra-substituters = http://127.0.0.1:7745/cache?compression=none\n" +
                        "extra-trusted-public-keys = ${nix-cache-proxy-key.public}\n" +
                        "post-build-hook = /local/post-build-hook\n" +
                        .NIX_CONFIG
                      ),
                  } |
                  .job[]?.group[]?.task[]?.template |= . + [{
                    destination: "local/post-build-hook",
                    perms: "544",
                    data: env.postBuildHook,
                  }] |
                  .job[]?.group[]?.task[]?.config.packages |= . + ["github:NixOS/nixpkgs/${nixpkgs.rev}#bash"] |
                  .job[]?.group[]?.task[]?.vault.policies |= . + ["cicero"]
                '';
              in prev.writers.writeDashBin "dev-cicero-transformer" ''
                export postBuildHook=${prev.lib.escapeShellArg post-build-hook}
                jq ${prev.lib.escapeShellArg filter}
              '';
            })
            self.overlay
          ]);
        in
        {
          packages = self.overlay (pkgs // self.packages.${system}) pkgs;
          defaultPackage = self.packages.${system}.cicero;
          hydraJobs = self.packages.${system};
          devShell = pkgs.devshell.fromTOML ./devshell.toml;
        }) // {
      overlay = final: prev: {
        cicero = prev.callPackage pkgs/cicero { flake = self; };
        cicero-entrypoint = prev.callPackage pkgs/cicero/entrypoint.nix { };
        cicero-evaluator-nix = prev.callPackage pkgs/cicero/evaluators/nix { flake = self; };
        webhook-trigger = prev.callPackage pkgs/trigger { };
      } // nixpkgs.lib.mapAttrs'
        (k: nixpkgs.lib.nameValuePair "cicero-evaluator-nix-run-${k}")
        (import pkgs/cicero/evaluators/nix/runners.nix prev);

      nixosModules.cicero = import nixos/modules/cicero.nix;
      nixosModule = self.nixosModules.cicero;

      nixosConfigurations = {
        dev = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [ nixos/configs/dev.nix ];
        };

        cicero = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [ nixos/configs/cicero.nix ];
        };
      };

      lib = import ./lib.nix self;

      ciceroActions = self.lib.callActionsWithExtraArgs
        rec {
          inherit (self.lib) std;
          inherit (self.inputs.nixpkgs) lib;
          actionLib = import ./action-lib.nix { inherit std lib; };
          nixpkgsRev = self.inputs.nixpkgs.rev;
        } ./actions;
    };
}
