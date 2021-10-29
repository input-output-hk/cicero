{ flake, writers, coreutils }:

writers.writeBashBin "cicero-evaluator-nix" ''
  PATH="$PATH:"${coreutils}/bin

  function usage {
    {
      echo    "Usage: $(basename "$0") <list|eval> <workflows-dir>"
      echo
      echo    'For eval, the following env vars must be set:'
      echo -e '\t- CICERO_WORKFLOW_NAME'
      echo -e '\t- CICERO_WORKFLOW_INSTANCE_ID'
      echo -e '\t- CICERO_WORKFLOW_INPUTS'
    } >&2
  }

  dir="''${2:-}"

  function evaluate {
      nix-instantiate \
          --eval \
          --strict \
          --json \
          --arg cicero 'builtins.getFlake (toString ${flake})' \
          --argstr dir "$(realpath "$dir")" \
          "$@"
  }

  case "''${1:-}" in
    list )
        evaluate \
          --expr '{ id, inputs, cicero, dir } @ args: builtins.attrNames (import ${./lib.nix} args)' \
          --argstr id 0 \
          --argstr inputs '{}'
        ;;
    eval )
        evaluate \
            ${./lib.nix} \
            --argstr id "$CICERO_WORKFLOW_INSTANCE_ID" \
            --argstr inputs "$CICERO_WORKFLOW_INPUTS" \
            --attr "$CICERO_WORKFLOW_NAME"
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
