{ lib, writers, curl, cacert, jq, vault }:

writers.writeBash "cicero-std_github_status" ''
  set -euo pipefail

  if [[ -n "''${CICERO_STD_DEBUG:-}" ]]; then
    set -x
  fi

  export PATH="$PATH:${lib.makeBinPath [ curl jq vault ]}"
  export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"

  url="$1"
  shift

  # TODO Only get from vault. Env var is just for development.
  if [[ -z "''${GITHUB_TOKEN:-}" ]]; then
    GITHUB_TOKEN=$(vault kv get -field=token kv/data/cicero/github)
  fi

  function cleanup {
    rm "$secret_headers"
  }
  trap cleanup EXIT

  secret_headers="$(mktemp)"

  cat >> "$secret_headers" <<EOF
  Authorization: token $GITHUB_TOKEN
  EOF

  jq "$@" -nc '{
    state: $state,
    context: "\($workflow_name):\($action_name)",
    description: $description,
    target_url: "\(env.CICERO_WEB_URL)/workflow/\($workflow_id)#\($action_name)",
  }' \
  | curl "$url" \
    -H 'Accept: application/vnd.github.v3+json' \
    -H @"$secret_headers" \
    --data-binary @-
''
