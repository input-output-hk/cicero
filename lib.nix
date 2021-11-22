self:

/* A workflow is a function of the form:

   ```nix
   { id, run }: {
     name = "clock"; # optional

     version = 1; # optional

     actions = {
       tick = { tick ? null }: {
         when."hasn't run yet" = tick != null;
         job = run "bash" {} ''
             echo tick
         '';
       };
     };
   }
   ```
*/

let inherit (self.inputs.nixpkgs) lib;

in rec {
  # Calls a workflow or file containing a workflow.
  callWorkflow = workflow:
    { id ? null, inputs ? { } }:
    let
      inherit (builtins)
        all attrNames attrValues fromJSON functionArgs typeOf mapAttrs;

      parsedInputs = {
        "set" = inputs;
        "string" = builtins.fromJSON inputs;
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

      mkActionState = { workflowName, actionName, job, inputs, when ? { }
        , success ? { ${actionName} = true; }
        , failure ? { ${actionName} = false; } }: {
          inherit when inputs success failure;
          job = hydrateNomadJob {
            "${workflowName}/${actionName}" =
              lib.optionalAttrs (all (x: x == true) (attrValues when)) job;
          };
        };

      transformWorkflow = { name, version ? null, actions ? { }, meta ? { } }:
        let
          transformAction = actionName: action:
            let
              inputNames = attrNames (functionArgs action);
              intersection =
                lib.intersectLists inputNames (attrNames parsedInputs);
              filteredInputs = lib.listToAttrs (map (inputName:
                lib.nameValuePair inputName (parsedInputs.${inputName} or null))
                intersection);
            in mkActionState ({
              inherit actionName;
              inputs = inputNames;
              workflowName = name;
            } // (action filteredInputs));

          transformedActions = mapAttrs transformAction actions;
        in {
          inherit name version meta;
          actions = transformedActions;
        };

      run = language: options: script:
        let groupAndTaskName = "run-${language}";
        in {
          group.${options.group or groupAndTaskName}.task.${
            options.task or groupAndTaskName
          } = {
            resources = {
              memory = options.memory or 300;
              cpu = options.cpu or null;
              cores = options.cores or null;
            };
            config = let runner = "run-${language}";
            in {
              packages = [ "github:input-output-hk/cicero/main#${runner}" ]
                ++ (options.packages or [ ]);
              command = [
                # It is ok to hard-code the system here
                # because we only care about the derivation name.
                "/bin/${
                  self.outputs.legacyPackages.x86_64-linux.${runner}.name
                }"
                "/local/script"
              ];
            };
            template = [{
              destination = "local/script";
              left_delimiter = "";
              right_delimiter = "";
              data = script;
            }];
          };
        };
    in transformWorkflow (importedWorkflow { inherit id run; });

  # Recurses through a directory, considering every file a workflow.
  # The path of the file from the starting directory is used as default
  # name if the evaluated workflow does not return a name itself.
  listWorkflows = dir:
    lib.listToAttrs (map (file:
      let
        defaultName = lib.pipe file [
          toString
          (lib.removePrefix "${toString dir}/")
          (lib.removeSuffix ".nix")
        ];
        inherit (workflowWithDefaults { name = defaultName; } (import file) {
          # We only evaluate the name so we can pass invalid args for the rest.
          id = null;
          run = null;
        })
          name;
      in lib.nameValuePair name file) (lib.filesystem.listFilesRecursive dir));

  # Like `listWorkflows` but calls every workflow.
  callWorkflows = dir:
    builtins.mapAttrs (name: wf:
      let wfWithName = workflowWithDefaults { inherit name; } (import wf);
      in callWorkflow wfWithName) (listWorkflows dir);

  callWorkflowsWithDefaults = defaults: dir:
    builtins.mapAttrs (k: workflowWithDefaults defaults) (callWorkflows dir);

  workflowWithDefaults = defaults: innerWorkflow: args:
    defaults
    // builtins.mapAttrs (k: v: if v == null then defaults.${k} or null else v)
    (innerWorkflow args);
}
