# shellcheck shell=bash

if [[ -v CICERO_EVALUATOR_NIX_VERBOSE ]]; then
	set -x
fi

export NIX_CONFIG="${NIX_CONFIG:-}"$'\n''extra-experimental-features = nix-command flakes'

function usage {
	{
		echo "Usage: $(basename "$0") [list] [eval <attrs...>]"
		echo
		echo 'For eval, the following env vars must be set:'
		echo -e '\t- CICERO_ACTION_NAME'
		echo -e '\t- CICERO_ACTION_ID'
		echo -e '\t- CICERO_ACTION_INPUTS'
		echo
		echo 'For eval, the following env vars must be set'
		echo 'in order to run respective preparation hooks:'
		echo -e '\t- CICERO_EVALUATOR_NIX_OCI_REGISTRY'
		echo -e '\t- CICERO_EVALUATOR_NIX_BINARY_CACHE'
		echo
		echo 'The following env vars are optional:'
		echo -e '\t- CICERO_EVALUATOR_NIX_EXTRA_ARGS'
		echo -e '\t- CICERO_EVALUATOR_NIX_VERBOSE'
		echo -e '\t- CICERO_EVALUATOR_NIX_STACKTRACE'
		echo -e '\t- CICERO_EVALUATOR_NIX_OCI_REGISTRY_SKOPEO_COPY_ARGS'
	} >&2
}

function msg {
	local event=${1:?'No event given'}
	shift

	local json='{'
	for pair in event="$event" "$@"; do
		local key=${pair%%=*}
		local val=${pair#*=}

		json+='"'"$key"'":'
		case ${val:0:1} in
		'{') ;&
		'[') ;&
		'0') ;& '1') ;& '2') ;& '3') ;& '4') ;& '5') ;& '6') ;& '7') ;& '8') ;& '9')
			json+="$val"
			;;
		*)
			json+='"'"$val"'"'
			;;
		esac
		json+=','
	done
	json="${json%,}"
	json+='}'

	echo "$json"
}

echo >&2 'Getting current system…'
system=$(nix eval --impure --raw --expr __currentSystem)
echo >&2 "Got current system: $system"

function evaluate {
	echo >&2 'Evaluating…'
	nix eval --no-write-lock-file --json \
		${CICERO_EVALUATOR_NIX_STACKTRACE:+--show-trace} \
		"$@"
}

function prepare {
	while read -r step; do
		local type
		type=$(<<< "$step" jq -r .type)

		echo >&2 "Preparing $type…"

		case "$type" in
		nix2container)
			local name imageDrv outputs
			name=$(<<<"$step" jq -r .name)
			imageDrv=$(<<<"$step" jq -r .imageDrv)

			local -a msgPairs=(prepare type="$type" name="$name")

			msg "${msgPairs[@]}" step=build imageDrv="$imageDrv"
			outputs=$(nix build --json "$imageDrv" | jq -r '.[].outputs.out')

			for image in $outputs; do
				echo >&2 "Pushing $image to $name…"
				msg "${msgPairs[@]}" step=push image="$image"
				#shellcheck disable=SC2086
				>&2 skopeo --insecure-policy copy ${CICERO_EVALUATOR_NIX_OCI_REGISTRY_SKOPEO_COPY_ARGS:-} \
					nix:"$image" "$name"
			done
			;;
		nix)
			local -a derivations
			readarray -t derivations < <(<<<"$step" jq -r '.derivations[]')

			local -a msgPairs=(prepare type="$type")

			msg "${msgPairs[@]}" step=build derivations="${derivations[*]}"
			local -a outputs
			readarray -t outputs < <(nix build --json "${derivations[@]}" | jq -r '.[].outputs[]')

			echo >&2 "Pushing to $CICERO_EVALUATOR_NIX_BINARY_CACHE: ${outputs[*]}…"
			msg "${msgPairs[@]}" step=push paths="${outputs[*]}"
			nix copy --to "$CICERO_EVALUATOR_NIX_BINARY_CACHE" "${outputs[@]}"
			;;
		*)
			local error='Unknown type "'"$type"'" in prepare step'
			echo >&2 "$error: $step"
			msg error error="$error" prepare="$step"
			exit 1
			;;
		esac
	done
}

case "${1:-}" in
list)
	shift
	msg result result="$(evaluate ".#cicero.$system" --apply __attrNames)"
	;;
eval)
	shift

	echo >&2 'Evaluating variables…'
	vars=$(
		nix-instantiate --eval --strict \
			--expr '{...} @ vars: {
			  # We are not using this ourselves but make it available for CICERO_EVALUATOR_NIX_EXTRA_ARGS.
			  inherit (vars) system;

			  name = __getEnv "CICERO_ACTION_NAME";

			  args = __mapAttrs (k: v: if v == "" then null else v) {
			    id          = __getEnv "CICERO_ACTION_ID";
			    ociRegistry = __getEnv "CICERO_EVALUATOR_NIX_OCI_REGISTRY";
			    inputs = let
			      env = __getEnv "CICERO_ACTION_INPUTS";
			    in
			      if env != ""
			      then __fromJSON env
			      else null;
			  };

			  attrs =
			    let requestedAttrs = __filter __isString (__split "[[:space:]]" vars.attrs); in
			    requestedAttrs ++
			    # Add "prepare" to the list of attributes to evaluate
			    # if "job" is present so that the preparation hooks can run.
			    (if __elem "job" requestedAttrs then [ "prepare" ] else []);
			}' \
			--argstr system "$system" \
			--argstr attrs "${*}"
	)

	trap 'rm -rf "$tmpFlake"' ERR
	tmpFlake=$(mktemp -d)
	cat > "$tmpFlake/flake.nix" <<-EOF
		{
		  inputs.source.url = "$PWD";

		  outputs = { source, ... }: {
		    result =
		      with $vars;

		      let
		        # in a separate let-block so it cannot access all the stuff below
		        allArgs = args // ${CICERO_EVALUATOR_NIX_EXTRA_ARGS:-"{}"};
		      in

		      let
		        mapAttrs' = fn: attrs: __listToAttrs (
		          __filter
		            (kv: kv.name != null)
		            (map
		              (name: fn name attrs.\${name})
		              (__attrNames attrs)
		            )
		        );

		        actionFn = source.outputs.cicero.$system.\${name};
		        actionFnArgs =
		          # If the action takes named args,
		          # provide only those requested (like callPackage).
		          # If it does not, simply provide everything.
		          let fnArgs = __functionArgs actionFn; in
		          if fnArgs == {} then allArgs else mapAttrs' (k: v: {
		            name = if allArgs.\${k} or null == null then null else k;
		            value = allArgs.\${k} or null;
		          }) fnArgs;
		        action = actionFn actionFnArgs;
		      in

		      mapAttrs' (k: value: {
		        name = if __elem k attrs then k else null;
		        inherit value;
		      }) action;
		  };
		}
	EOF
	result=$(evaluate "$tmpFlake#result")
	rm -r "$tmpFlake"
	trap ERR

	msg result result="$result"

	echo -n "$result" | jq -c '.prepare[]?' | prepare
	;;
*)
	if [[ -n "${1:-}" ]]; then
		error="Unknown command: $1"
	else
		error='No command given'
	fi

	msg error error="$error"

	echo >&2 "$error"
	echo >&2
	usage

	exit 1
	;;
esac
