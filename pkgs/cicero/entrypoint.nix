{
  lib,
  cicero,
  cicero-evaluator-nix,
  writeShellScriptBin,
  nix,
  bashInteractive,
  coreutils,
  shadow,
  gitMinimal,
  cacert,
  dbmate,
  vault-bin,
  netcat,
  ...
}:
writeShellScriptBin "entrypoint" ''
  set -exuo pipefail

  export PATH="${
    lib.makeBinPath [
      gitMinimal
      cicero
      cicero-evaluator-nix
      nix
      bashInteractive
      coreutils
      dbmate
      vault-bin
      netcat
    ]
  }"

  export NIX_CONFIG="experimental-features = nix-command flakes"
  export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"

  mkdir -p /etc
  echo 'nixbld:x:30000:nixbld1' > /etc/group
  echo 'nixbld1:x:30001:30000:Nix build user 1:/var/empty:${shadow}/bin/nologin' > /etc/passwd
  nix-store --load-db < /registration

  echo "nameserver ''${NAMESERVER:-172.17.0.1}" > /etc/resolv.conf

  if [ -n "''${NAMESERVER:-}" ]; then
    echo "nameserver ''${NAMESERVER:-}" > /etc/resolv.conf
  else
    defaultNameservers=(172.17.0.1 127.0.0.1 1.1.1.1)

    for ns in "''${defaultNameservers[@]}"; do
      if nc -z -w 3 "$ns" 53; then
        echo "nameserver ''${NAMESERVER:-$ns}" > /etc/resolv.conf
        break
      fi
    done
  fi

  dbmate \
    --migrations-dir ${../../db/migrations} \
    --no-dump-schema \
    --wait \
    up

  if [ -n "''${VAULT_TOKEN:-}" ]; then
    set +x
    NOMAD_TOKEN="$(vault read -field secret_id nomad/creds/cicero)"
    export NOMAD_TOKEN
    set -x
  else
    echo "No VAULT_TOKEN set, skipped obtaining a Nomad token"
  fi

  exec cicero start "$@"
''
