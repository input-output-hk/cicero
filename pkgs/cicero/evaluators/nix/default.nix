{ flake, writers, coreutils }:

writers.writeBashBin "cicero-evaluator-nix" ''
  PATH="$PATH:"${coreutils}/bin

  function usage {
      {
          echo    "Usage: $(basename "$0") [list] [eval <attrs...>]"
          echo
          echo    'The following env vars must be set:'
          echo -e '\t- CICERO_ACTION_SRC'
          echo
          echo    'For eval, the following env vars must be set:'
          echo -e '\t- CICERO_ACTION_NAME'
          echo -e '\t- CICERO_ACTION_ID'
          echo -e '\t- CICERO_ACTION_INPUTS'
      } >&2
  }

  function evaluate {
      nix eval --json "$CICERO_ACTION_SRC"#ciceroActions "$@"
  }

  case "''${1:-}" in
    list )
        evaluate --apply builtins.attrNames
        ;;
    eval )
        # XXX get rid of --impure
        shift
        attrs="$@" \
        evaluate --impure --apply '
          actions:

          let
            inherit (builtins) getEnv filter isString split attrNames elem;

            action = actions.''${getEnv "CICERO_ACTION_NAME"} {
              id = getEnv "CICERO_ACTION_ID"; # TODO "" => null?
              inputs = getEnv "CICERO_ACTION_INPUTS"; # TODO fromJSON() here
            };

            attrs = filter isString (split "[[:space:]]+" (getEnv "attrs"));
          in

          builtins.listToAttrs (map
            (name: {
              inherit name;
              value = action.''${name};
            })
            (filter
              (name: elem name attrs)
              (attrNames action)
            )
          )
        '
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
