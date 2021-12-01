self:

/* A workflow is a function of the form:

   ```nix
   { std }: { name, id }: {
     actions.tick = { tick ? null }: {
       when."hasn't run yet" = tick != null;
       job = std.job.default (std.task.script "bash" {} ''
         echo tick
       '');
     };
   }
   ```
*/

let inherit (self.inputs.nixpkgs) lib;

in rec {
  # Calls a workflow or file containing a workflow.
  callWorkflow = name: workflow:
    { id ? null, inputs ? { } }:
    let
      inherit (builtins)
        all attrNames attrValues fromJSON typeOf mapAttrs;

      parsedInputs = {
        "set" = inputs;
        "string" = fromJSON inputs;
      }.${typeOf inputs};

      importedWorkflow =
        if builtins.isFunction workflow then workflow else import workflow;

      hydrateNomadJob = mapAttrs (k: job:
        assert !(job ? ID); # workflow authors must not set an ID
        lib.recursiveUpdate job ({
          type = "batch";
        } // lib.optionalAttrs (job ? group) {
          group = mapAttrs (k: group:
            lib.recursiveUpdate group (lib.optionalAttrs (group ? task) {
              task =
                mapAttrs (k: task: lib.recursiveUpdate { driver = "nix"; } task)
                group.task;
            })) job.group;
        }));

      mkActionState = { actionName, job, inputs, when ? { }
        , success ? { ${actionName} = true; }
        , failure ? { ${actionName} = false; } }: {
          inherit when inputs success failure;
          job = hydrateNomadJob {
            "${name}/${actionName}" =
              lib.optionalAttrs (all (x: x == true) (attrValues when)) job;
          };
        };

      mkWorkflowState = { actions ? { }, meta ? { } }: {
        inherit meta;
        actions = mapAttrs (actionName: action:
          let
            inputNames = attrNames (lib.functionArgs action);

            intersection =
              lib.intersectLists inputNames (attrNames parsedInputs);

            filteredInputs = lib.listToAttrs (map (inputName:
              lib.nameValuePair inputName (parsedInputs.${inputName} or null))
              intersection);
          in mkActionState ({
            inherit actionName;
            inputs = inputNames;
          } // action filteredInputs)) actions;
      };
    in mkWorkflowState (importedWorkflow { inherit name id; });

  # Recurses through a directory, considering every file a workflow.
  # The path of the file from the starting directory is used as name.
  listWorkflows = dir:
    lib.listToAttrs (map (file:
      lib.nameValuePair (lib.pipe file [
        toString
        (lib.removePrefix "${toString dir}/")
        (lib.removeSuffix ".nix")
      ]) file) (lib.filesystem.listFilesRecursive dir));

  # Like `listWorkflows` but calls every workflow.
  callWorkflows = dir: builtins.mapAttrs callWorkflow (listWorkflows dir);

  callWorkflowsWithDefaults = defaults: dir:
    builtins.mapAttrs (k: workflowWithDefaults defaults) (callWorkflows dir);

  callWorkflowsWithExtraArgs = extras: dir:
    builtins.mapAttrs (k: v: callWorkflow k (workflowWithExtraArgs extras (import v))) (listWorkflows dir);

  workflowWithDefaults = defaults: innerWorkflow: args:
    defaults
    // builtins.mapAttrs (k: v: if v == null then defaults.${k} or null else v)
    (innerWorkflow args);

  workflowWithExtraArgs = extras: innerWorkflow: args:
    innerWorkflow (args // extras);

  std = import ./pkgs/cicero/evaluators/nix/lib.nix self;
}
