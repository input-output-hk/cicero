# shellcheck shell=bash

if [[ -n "${CICERO_EVALUATOR_NIX_LOG:-}" ]]; then
	set -x
fi

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
		echo 'For eval, the following env vars must be set'
		echo 'in order to run respective preparation hooks:'
		echo -e '\t- CICERO_EVALUATOR_NIX_OCI_REGISTRY'
		echo
		echo 'The following env vars are optional:'
		echo -e '\t- CICERO_EVALUATOR_NIX_LOG'
		echo -e '\t- CICERO_EVALUATOR_NIX_STACKTRACE'
	} >&2
}

system=$(nix eval --impure --raw --expr __currentSystem)

function evaluate {
	nix eval --no-write-lock-file --json \
		${CICERO_EVALUATOR_NIX_STACKTRACE:+--show-trace} \
		"${CICERO_ACTION_SRC:-.}#cicero.$system" "$@"
}

function prepare {
	while read -r step; do
		local type
		type=$(<<< "$step" jq -r .type)

		echo "Preparing $type…"

		case "$type" in
			nix2container)
				local name imageDrv
				name=$(<<<"$step" jq -r .name)
				imageDrv=$(<<<"$step" jq -r .imageDrv)
				for image in $(nix build --json "$imageDrv" | jq -r '.[].outputs.out'); do
					echo "Pushing $image to $name…"
					skopeo --insecure-policy copy nix:"$image" "$name"
				done
				;;
			*)
				echo 'Unknown type of prepare step: "'"$step"\"
				exit 1
				;;
		esac
	done >&2
}

case "${1:-}" in
list)
	evaluate --apply __attrNames
	;;
eval)
	shift

	vars=$(
		nix-instantiate --eval --strict \
			--expr '{...} @ vars: {
			  args = __mapAttrs (k: v: if v == "" then null else v) {
			    id = vars.id;
			    ociRegistry = vars.ociRegistry;
			  } // {
			    inputs = __fromJSON vars.inputs;
			  };

			  inherit (vars) name;
			  attrs =
			    let requestedAttrs = __filter __isString (__split "[[:space:]]" vars.attrs); in
			    requestedAttrs ++
			    # Add "prepare" to the list of attributes to evaluate
			    # if "job" is present so that the preparation hooks can run.
			    (if __elem "job" requestedAttrs then [ "prepare" ] else []);
			}' \
			--argstr name "${CICERO_ACTION_NAME:-}" \
			--argstr id "${CICERO_ACTION_ID:-}" \
			--argstr inputs "${CICERO_ACTION_INPUTS:-null}" \
			--argstr ociRegistry "${CICERO_EVALUATOR_NIX_OCI_REGISTRY:-}" \
			--argstr attrs "${*}"
	)

	result=$(evaluate --apply "$(
		cat <<-EOF
			actions:
			let
			  mapAttrs' = fn: attrs: __listToAttrs (
			    __filter
			      (kv: kv.name != null)
			      (map
			        (name: fn name attrs.\${name})
			        (__attrNames attrs)
			      )
			  );

			  vars = $vars;

			  actionFn = actions.\${vars.name};
			  actionFnArgs =
			    # If the action takes named args
			    # provide only those requested (like callPackage).
			    # If it does not simply provide everything.
			    let args = __functionArgs actionFn; in
			    if args == {} then vars.args else mapAttrs' (k: v: {
			      name = if vars.args.\${k} or null == null then null else k;
			      value = vars.args.\${k} or null;
			    }) args;
			  action = actionFn actionFnArgs;
			in
			  mapAttrs' (k: value: {
			    name = if __elem k vars.attrs then k else null;
			    inherit value;
			  }) action
		EOF
	)")

	echo -n "$result"

	echo -n "$result" | jq -c '.prepare[]?' | prepare >&2
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
