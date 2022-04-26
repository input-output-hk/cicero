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
	evaluate --apply __attrNames
	;;
eval)
	shift
	vars="$(
		nix-instantiate --eval --strict \
			--expr '{...}@args: args // {
			  id = if args.id == "" then null else args.id;
			  inputs = __fromJSON args.inputs;
			  attrs = __filter __isString (__split "[[:space:]]" args.attrs);
			}' \
			--argstr inputs "${CICERO_ACTION_INPUTS:-null}" \
			--argstr name "${CICERO_ACTION_NAME:-}" \
			--argstr id "${CICERO_ACTION_ID:-}" \
			--argstr attrs "${*}"
	)"

	evaluate --apply "$(
		cat <<-EOF
			actions:
			let
			  inherit (${vars}) attrs id inputs name;
			  nonNullAttr = k: v: if v == null then {} else { \${k} = v; };
			  action = actions.\${name} (
			    nonNullAttr "id" id //
			    nonNullAttr "inputs" inputs
			  );
			in
			  __listToAttrs (map
			    (name: { inherit name; value = action.\${name}; })
			    (__filter (name: __elem name attrs) (__attrNames action)))
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