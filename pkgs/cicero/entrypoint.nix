{
  lib,
  cicero,
  cicero-evaluator-nix,
  writeShellApplication,
  nix,
  bash,
  coreutils,
  shadow,
  gitMinimal,
  cacert,
  dbmate,
  vault-bin,
  netcat,
  ...
}:
writeShellApplication {
  name = "entrypoint";

  runtimeInputs = [
    (gitMinimal.override {perlSupport = false;})
    cicero
    cicero-evaluator-nix
    nix
    bash
    coreutils
    dbmate
    vault-bin
    netcat
  ];

  text = ''
    set -x

    export NIX_CONFIG="
    experimental-features = nix-command flakes
    ''${NIX_CONFIG:-}
    "
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

    set +x
    if [ -n "''${VAULT_TOKEN:-}" ]; then
      NOMAD_TOKEN="$(vault read -field secret_id nomad/creds/cicero)"
      export NOMAD_TOKEN
    else
      echo "No VAULT_TOKEN set, skipped obtaining a Nomad token"
    fi
    set -x

    exec cicero start "$@"
  '';
}
