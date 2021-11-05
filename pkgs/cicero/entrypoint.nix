{ lib, cicero, cicero-evaluator-nix, writeShellScriptBin, wfs, nixUnstable
, bashInteractive, coreutils, shadow, git, cacert, dbmate, vault-bin, ... }:
writeShellScriptBin "entrypoint" ''
  set -exuo pipefail

  export PATH="${
    lib.makeBinPath [
      git
      cicero
      cicero-evaluator-nix
      wfs
      nixUnstable
      bashInteractive
      coreutils
      dbmate
      vault-bin
    ]
  }"

  export NIX_CONFIG="experimental-features = nix-command flakes"
  export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"

  mkdir -p /etc
  echo 'nixbld:x:30000:nixbld1' > /etc/group
  echo 'nixbld1:x:30001:30000:Nix build user 1:/var/empty:${shadow}/bin/nologin' > /etc/passwd
  echo "nameserver ''${NAMESERVER:-172.17.0.1}" > /etc/resolv.conf
  nix-store --load-db < /registration

  if [ -d cicero ]; then
    git -C cicero pull
  else
    git clone --quiet --depth 1 https://github.com/input-output-hk/cicero
  fi

  cd cicero
  dbmate up

  if [ -n "''${VAULT_TOKEN:-}" ]; then
    NOMAD_TOKEN="$(vault read -field secret_id nomad/creds/cicero)"
    export NOMAD_TOKEN
  else
    echo "No VAULT_TOKEN set, skipped obtaining a Nomad token"
  fi

  exec /bin/cicero all "$@"
''
