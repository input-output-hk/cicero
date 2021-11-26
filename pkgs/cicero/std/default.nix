{ callPackage, writers }:

writers.writeBashBin "cicero-std" ''
  set -euo pipefail

  case "$*" in
      'github status '* )
          shift 2
          ${callPackage ./github-status.nix { }} "$@"
          ;;
  esac
''
