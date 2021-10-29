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

  hydrateNomadJob = { workflowName, stepName, job }:
    assert !(job ? ID); # workflow authors must not set an ID
    lib.recursiveUpdate job {
      Name = "${workflowName}/${stepName}";

      type = "batch";

      TaskGroups = map (group:
        lib.recursiveUpdate group {
          Name = stepName;

          Tasks = map (task:
            lib.recursiveUpdate {
              Name = stepName;
              Driver = "nix";
              Config = lib.recursiveUpdate {
                resolv_conf = "copy-host";
                boot = false;
                user_namespacing = false;
                network_veth = false;
                console = "read-only";
                ephemeral = true;
                process_two = false;
                volatile = "overlay";
              } task.Config;
            } task) group.Tasks;
        }) job.TaskGroups;
    };

  run = language: script: {
    TaskGroups = [{
      Tasks = [{
        Config = let runner = "run-${language}";
        in assert pkgs ? ${runner}; {
          packages = [ "github:input-output-hk/cicero#${runner}" ];
          command = [ "/bin/${pkgs.${runner}.name}" script ];
        };
      }];
    }];
  };

  mkStepState = { workflowName, stepName, job, inputs, when ? { }
    , success ? { ${stepName} = true; }, failure ? { ${stepName} = false; } }: {
      inherit when inputs success failure;
      job = hydrateNomadJob {
        inherit workflowName stepName;
        job = {
          TaskGroups = [ ];
        } // lib.optionalAttrs (all lib.id (attrValues when)) job;
      };
    };

  workflow = { name, version ? 0, steps ? { }, meta ? { } }:
    let
      transformStep = stepName: step:
        let
          inputNames = attrNames (functionArgs step);
          intersection = lib.intersectLists inputNames (attrNames parsedInputs);
          filteredInputs = lib.listToAttrs (map (inputName:
            lib.nameValuePair inputName (parsedInputs.${inputName} or null))
            intersection);
        in mkStepState ({
          inherit stepName;
          inputs = inputNames;
          workflowName = name;
        } // (step filteredInputs));

      transformedSteps = lib.mapAttrs transformStep steps;
    in {
      inherit name meta version;
      steps = transformedSteps;
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
