self:

let
  inherit (self.inputs.nixpkgs) lib;
  inherit (self.inputs) data-merge;
in rec {
  workflows = rec {
    /* Convenience for defining and wrapping a workflow.

       Takes an attribute set of "spec workflow" arguments
       as its first argument but drops any unknown keys.

       The second argument is a "std workflow".

       This allows access to any possible extra args before
       calling `mkWorkflow` with the "spec workflow" args.

       Useful if you get std itself passed as an extra arg,
       as you need to access std to call `mkWorkflow`.
    */
    callWorkflow = { name, id, ... }@args:
      lib.flip mkWorkflow { inherit name id; };

    /* Takes a "std workflow" and returns a "spec workflow".

       The "spec workflow" is a function that takes a set
       `{ name, id }` as its only argument and returns
       the final JSON that the cicero evaluator parses.

       The "std workflow" is the set or function
       which layout is specific to `mkWorkflow`.
       It is similar to a "spec workflow".
       I'll try to briefly visualize it here:

       ```nix
       [workflow:] {
         actions = [actions:] {
           <name> = [action:] { factFoo ? null }: {
             when.conditionFoo = factFoo != null;
             job = …;
           };
         };
       }

       `job` is the JSON representation a Nomad job's HCL or
       a list that is passed to the `chain` function (see its docs).
       It may be null or absent entirely.

       The arguments in brackets are optional,
       meaning that either a function with a single argument
       or an attribute set is allowed in their place.

       If the function form is used, that first argument
       is the result of the function itself for recursive access
       with specific extra keys added:
       - workflow: { name, id, <self keys…> }
       - actions: { workflow, <self keys…> }
       - action: { actions, <self keys…> }

       This allows access to the parent structure
       from any place in the hierarchy.
       ```
    */
    mkWorkflow = wf:
      { name, id }@wfInfo:
      let
        wfFn = if builtins.typeOf wf == "set" then (_: wf) else wf;
        initWf = wfFn (wfFn wfInfo // wfInfo);
      in initWf // {
        actions = let
          actionsFn = if builtins.typeOf initWf.actions == "set" then
            (_: initWf.actions)
          else
            initWf.actions;
          actionsInfo = { workflow = initWf // wfInfo; };
          initActions = actionsFn (actionsFn actionsInfo // actionsInfo);
        in mkActions initActions actionsInfo;
      };

    mkActions = actions: actionsInfo:
      builtins.mapAttrs (mkAction (actions // actionsInfo)) actions;

    mkAction = actions: name: action:
      let
        # actionRaw = { fact ? null }: { when = …; job = …; }
        actionRaw = if builtins.typeOf (action { }) == "set" then
          action
        else
          (action { });
        actionRawArgs = builtins.functionArgs actionRaw;

        # actionFn = self: { fact ? null }: { when = …; job = …; }
        actionFn =
          if builtins.typeOf (action { }) == "set" then (_: action) else action;

        info = { inherit name actions; };

        actionFnArg = actionFn info { } // info;
        initAction = actionFn actionFnArg;

        wrapper = facts:
          let result = initAction facts;
          in result // {
            job = if result ? job then
              if builtins.typeOf result.job == "list" then
                chain actionFnArg result.job
              else
                result.job
            else
              null;
          };
        # We need to `setFunctionArgs` because that information is lost
        # as the `wrapper` just takes `facts:`, leading to no facts
        # being passed to this action as it appears like it takes none.
      in lib.setFunctionArgs wrapper actionRawArgs;
  };

  jobs.singleTask = { name, ... }: task: { group.${name}.task.${name} = task; };

  tasks.script = language: script:
    let
      runner = "run-${language}";
      scriptName = builtins.hashString "md5" script;
    in {
      config = {
        packages =
          [ "github:input-output-hk/cicero/${self.rev or ""}#${runner}" ];
        command = [
          # It is ok to hard-code the system here
          # because we only care about the derivation name.
          "/bin/${self.outputs.legacyPackages.x86_64-linux.${runner}.name}"
          "/local/scripts/${language}/${scriptName}"
        ];
      };

      template = [{
        destination = "local/scripts/${language}/${scriptName}";
        left_delimiter = "";
        right_delimiter = "";
        data = script;
      }];
    };

  /* Chains are a concise way to write jobs.
     Put simply chains are a fold-right of wrappers.

     Each "step/link/part" (no name defined) of a chain
     is a function that takes the action that this job
     is being defined for as its first argument
     and the next "step/link/part" as its second argument.
     These argument are supplied automatically when called
     by the `chain` function.
     It returns the job as an attribute set.

     Most "steps/links/parts" are created from a builder function
     that takes some specific arguments which are usually given
     directly in the workflow definition.

     This simple contract sometimes allows to use other functions
     that are not primarily meant to be used in a chain, or use
     functions that are meant for chains by themselves.
  */
  chains = {
    /* The main entrypoint to chains.

       For simplicity, plain attribute sets
       are also allowed. They will simply be merged
       with the next (if any) "step/link/part"
       using `data-merge.merge`.
    */
    chain = action: steps:
      lib.foldr (a: b:
        if builtins.typeOf a == "set" then
          data-merge.merge b a
        else
          (a action) b) { } steps;

    tasks = {
      /* Like `script` but the second argument is
         a function that takes the command of the
         next script and returns the new script.

         Example:

         ```nix
         wrapScript "bash" (inner: ''
           echo 'Running …'
           time ${lib.escapeShellArgs inner}
           echo '… finished.'
         '')
         ```
      */
      wrapScript = language: outerFn: action: inner:
        let outer = script language (outerFn inner.config.command or [ ]);
        in data-merge.merge (lib.recursiveUpdate inner {
          config.command = outer.config.command;

          # XXX we have to pre-create these keys because they may not be present
          # see https://github.com/divnix/data-merge/issues/1
          config.packages = inner.config.packages or [ ];
          template = inner.template or [ ];
        }) {
          config.packages = data-merge.append outer.config.packages;
          template = data-merge.append outer.template;
        };

      nix = {
        install = action: next:
          data-merge.merge (lib.recursiveUpdate next {
            # XXX we have to pre-create `config.packages` because it may not be present
            # see https://github.com/divnix/data-merge/issues/1
            config.packages = next.config.packages or [ ];
          }) {
            config.packages = data-merge.append
              [ "github:input-output-hk/nomad-driver-nix/wrap-nix#wrap-nix" ];
            env.NIX_CONFIG = ''
              experimental-features = nix-command flakes
              ${next.env.NIX_CONFIG or ""}
            '';
          };

        develop = action: next:
          nix.install action (wrapScript "bash" (next: ''
            nix --extra-experimental-features 'nix-command flakes' \
              develop -c ${lib.escapeShellArgs next}
          '') action next);

        build = action: next:
          nix.install action (wrapScript "bash" (next: ''
            if [[ -f flake.nix ]]; then
              nix build
            else
              nix-build
            fi
            ${lib.escapeShellArgs next}
          '') action next);
      };

      makes = target: action: next:
        data-merge.merge (wrapScript "bash" (next: ''
          m ${lib.escapeShellArg target}
          ${lib.escapeShellArgs next}
        '') action next) {
          config.packages = data-merge.append [ "github:fluidattacks/makes" ];
        };

      git.clone = { repo, sha, ... }:
        action: next:
        data-merge.merge (wrapScript "bash" (next: ''
          export SSL_CERT_FILE=/current-profile/etc/ssl/certs/ca-bundle.crt
          export HOME="$PWD/.home"

          mkdir -p "$HOME"

          git config --global advice.detachedHead false
          git clone --quiet ${lib.escapeShellArg repo.clone_url} src
          cd src
          git checkout ${lib.escapeShellArg sha}

          ${lib.escapeShellArgs next}
        '') action next) {
          config.packages = data-merge.append [
            "github:NixOS/nixpkgs/${self.inputs.nixpkgs.rev}#gitMinimal"
            "github:NixOS/nixpkgs/${self.inputs.nixpkgs.rev}#cacert"
          ];
        };

      github.reportStatus = statuses_url: action: next:
        data-merge.merge (wrapScript "bash" (inner: ''
          export SSL_CERT_FILE=/current-profile/etc/ssl/certs/ca-bundle.crt

          # TODO Only get from vault. Env var is just for development.
          if [[ -z "''${GITHUB_TOKEN:-}" ]]; then
            GITHUB_TOKEN=$(vault kv get -field=token kv/data/cicero/github)
          fi

          function cleanup {
            rm -f "$secret_headers"
          }
          trap cleanup EXIT

          secret_headers="$(mktemp)"

          cat >> "$secret_headers" <<EOF
          Authorization: token $GITHUB_TOKEN
          EOF

          function report {
            jq -nc '{
              state: $state,
              context: $action_name,
              description: $description,
              target_url: "\(env.CICERO_WEB_URL)/action/\($action_id)",
            }' \
              --arg state "$1" \
              --arg description "Run $NOMAD_JOB_ID" \
              --arg action_id ${lib.escapeShellArg action.id} \
              --arg action_name ${lib.escapeShellArg action.name} \
            | curl ${lib.escapeShellArg statuses_url} \
              > /dev/null --no-progress-meter \
              -H 'Accept: application/vnd.github.v3+json' \
              -H @"$secret_headers" \
              --data-binary @-
          }

          function err {
            report error
          }
          trap err ERR

          report pending

          if ${lib.escapeShellArgs inner}; then
            report success
          else
            status=$?
            report failure
            exit $status
          fi
        '') action next) {
          config.packages = data-merge.append [
            "github:NixOS/nixpkgs/${self.inputs.nixpkgs.rev}#curl"
            "github:NixOS/nixpkgs/${self.inputs.nixpkgs.rev}#jq"
            "github:NixOS/nixpkgs/${self.inputs.nixpkgs.rev}#vault"
            "github:NixOS/nixpkgs/${self.inputs.nixpkgs.rev}#cacert"
          ];
        };
    };
  };

  inherit (workflows) callWorkflow mkWorkflow;
  inherit (jobs) singleTask;
  inherit (tasks) script;
  inherit (chains) chain;
  inherit (chains.tasks) wrapScript nix makes git github;
  inherit data-merge;
}
