#!/bin/sh
# shellcheck shell=bash disable=SC1090

set -exu

case "${type:-bash}" in
  bash)
    . "${scriptPath:-}"
    ;;
  ruby)
    ruby "${scriptPath:-}"
    ;;
  python)
    python "${scriptPath:-}"
    ;;
  crystal)
    crystal run "${scriptPath:-}"
    ;;
  nomad)
    # TODO get NOMAD_API and NOMAD_TOKEN in here
    curl "$NOMAD_API/v1/jobs" \
      --header "X-Nomad-Token: $NOMAD_TOKEN" \
      --request POST --data "@${scriptPath:-}"
    ;;
esac

echo -n "${result:-}" > "${out:-}"
