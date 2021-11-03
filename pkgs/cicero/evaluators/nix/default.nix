{ flake, writers, coreutils }:

writers.writeBashBin "cicero-evaluator-nix" ''
  PATH="$PATH:"${coreutils}/bin

  function usage {
      {
          echo    "Usage: $(basename "$0") <list|eval>"
          echo
          echo    'The following env vars must be set:'
          echo -e '\t- CICERO_EVALUATOR_NIX_FLAKE'
          echo
          echo    'The following env var is optional:'
          echo -e '\t- CICERO_WORKFLOW_VERSION'
          echo
          echo    'For eval, the following env vars must be set:'
          echo -e '\t- CICERO_WORKFLOW_NAME'
          echo -e '\t- CICERO_WORKFLOW_INSTANCE_ID'
          echo -e '\t- CICERO_WORKFLOW_INPUTS'
      } >&2
  }

  function evaluate {
      local version=''${CICERO_WORKFLOW_VERSION:-}
      if [[ -n "''${version:-}" ]]; then
          version="?rev=$version"
      fi
      nix eval --json "$CICERO_EVALUATOR_NIX_FLAKE$version"#ciceroWorkflows "$@"
  }

  case "''${1:-}" in
    list )
        evaluate --apply builtins.attrNames
        ;;
    eval )
        # XXX get rid of --impure
        evaluate --impure \
            --apply 'wfs: wfs.''${builtins.getEnv "CICERO_WORKFLOW_NAME"} { id = builtins.getEnv "CICERO_WORKFLOW_INSTANCE_ID"; inputs = builtins.getEnv "CICERO_WORKFLOW_INPUTS"; }'
       ;;
    ''' )
        >&2 echo 'No command given'
        >&2 echo
        usage
        exit 1
        ;;
    * )
        >&2 echo "Unknown command: $1"
        >&2 echo
        usage
        exit 1
        ;;
  esac
''
