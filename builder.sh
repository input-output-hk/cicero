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
esac

echo -n "${result:-}" > "${out:-}"
