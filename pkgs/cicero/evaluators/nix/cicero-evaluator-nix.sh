# shellcheck shell=bash

function usage {
	{
		echo "Usage: $(basename "$0") [list] [eval <attrs...>]"
		echo
		echo 'The following env vars must be set:'
		echo -e '\t- CICERO_ACTION_SRC'
		echo
		echo 'For eval, the following env vars must be set:'
		echo -e '\t- CICERO_ACTION_NAME'
		echo -e '\t- CICERO_ACTION_ID'
		echo -e '\t- CICERO_ACTION_INPUTS'
		echo
		echo 'The following env vars are optional:'
		echo -e '\t- CICERO_EVALUATOR_NIX_STACKTRACE'
	} >&2
}

function evaluate {
	nix eval --no-write-lock-file --json \
		${CICERO_EVALUATOR_NIX_STACKTRACE:+--show-trace} \
		"${CICERO_ACTION_SRC:-.}#ciceroActions" "$@"
}

case "${1:-}" in
list)
	evaluate --apply builtins.attrNames
	;;
eval)
	shift
	vars="$(
		nix-instantiate --eval --strict \
			--expr '{...}@args: args // {
			  id = if args.id == "" then null else args.id;
			  inputs = builtins.fromJSON args.inputs;
			}' \
			--argstr inputs "${CICERO_ACTION_INPUTS:-null}" \
			--argstr name "${CICERO_ACTION_NAME:-}" \
			--argstr id "${CICERO_ACTION_ID:-}" \
			--arg attrs "[$(printf ' "%s"' "$@")]"
	)"

	evaluate --apply "$(
		cat <<-EOF
			actions:
			let
			  inherit (builtins) attrNames elem filter fromJSON isString listToAttrs split;
			  inherit (${vars}) attrs id inputs name;
			  nonNullAttr = k: v: if v == null then {} else { \${k} = v; };
			  action = actions.\${name} (
			    nonNullAttr "id" id //
			    nonNullAttr "inputs" inputs
			  );
			in
			  builtins.listToAttrs (map
			    (name: { inherit name; value = action.\${name}; })
			    (filter (name: elem name attrs) (attrNames action)))
		EOF
	)"
	;;
'')
	echo >&2 'No command given'
	echo >&2
	usage
	exit 1
	;;
*)
	echo >&2 "Unknown command: $1"
	echo >&2
	usage
	exit 1
	;;
esac
