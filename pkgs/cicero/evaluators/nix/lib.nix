{ cicero, id, inputs ? { }, dir }:

let
  inherit (builtins) all attrNames attrValues fromJSON functionArgs typeOf;

  parsedInputs = {
    "set" = inputs;
    "string" = builtins.fromJSON inputs;
  }.${typeOf inputs};

  pkgs = cicero.inputs.nixpkgs.legacyPackages.${builtins.currentSystem}
    // cicero.legacyPackages.${builtins.currentSystem};

  inherit (cicero.inputs.nixpkgs) lib;

  hydrateNomadJob = { workflowName, actionName, job }:
    assert !(job ? ID); # workflow authors must not set an ID
    lib.recursiveUpdate job {
      Name = "${workflowName}/${actionName}";

      type = "batch";

      TaskGroups = map (group:
        lib.recursiveUpdate group {
          Name = actionName;

          Tasks = map (task:
            lib.recursiveUpdate {
              Name = actionName;
              Driver = "nix";
              Config = task.Config;
            } task) group.Tasks;
        }) job.TaskGroups;
    };

  run = language: options: script: {
    inherit (options) Datacenters;
    TaskGroups = [{
      Tasks = [{
        Resources = {
          MemoryMB = options.memory or 300;
          CPU = options.cpu or null;
          Cores = options.cores or null;
        };
        Config = let runner = "run-${language}";
        in assert pkgs ? ${runner}; {
          packages = [ "github:input-output-hk/cicero/main#${runner}" ]
            ++ (options.packages or [ ]);
          command = [ "/bin/${pkgs.${runner}.name}" script ];
        };
      }];
    }];
  };

  mkActionState = { workflowName, actionName, job, inputs, when ? { }
    , success ? { ${actionName} = true; }, failure ? { ${actionName} = false; }
    }: {
      inherit when inputs success failure;
      job = hydrateNomadJob {
        inherit workflowName actionName;
        job = {
          TaskGroups = [ ];
        } // lib.optionalAttrs (all lib.id (attrValues when)) job;
      };
    };

  workflow = { name, version ? 0, actions ? { }, meta ? { } }:
    let
      transformAction = actionName: action:
        let
          inputNames = attrNames (functionArgs action);
          intersection = lib.intersectLists inputNames (attrNames parsedInputs);
          filteredInputs = lib.listToAttrs (map (inputName:
            lib.nameValuePair inputName (parsedInputs.${inputName} or null))
            intersection);
        in mkActionState ({
          inherit actionName;
          inputs = inputNames;
          workflowName = name;
        } // (action filteredInputs));

      transformedActions = lib.mapAttrs transformAction actions;
    in {
      inherit name meta version;
      actions = transformedActions;
    };

  workflows = dir:
    lib.listToAttrs (map (file:
      let
        name = lib.pipe file [
          toString
          (lib.removePrefix "${toString dir}/")
          (lib.removeSuffix ".nix")
        ];
        wf = workflow ({
          inherit name;
        } // (import file) {
          id = toString id;
          inherit run;
        });
      in lib.nameValuePair wf.name wf) (lib.filesystem.listFilesRecursive dir));

in workflows dir
