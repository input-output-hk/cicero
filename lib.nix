{ id, inputs ? {} }:

let
  inherit (builtins) all attrNames attrValues fromJSON functionArgs getFlake typeOf;

  parsedInputs = {
    "set" = inputs;
    "string" = builtins.fromJSON inputs;
  }.${typeOf inputs};

  flake = getFlake (toString ./.);
  pkgs =
    flake.inputs.nixpkgs.legacyPackages.${builtins.currentSystem}
    // flake.legacyPackages.${builtins.currentSystem};

  inherit (flake.inputs.nixpkgs) lib;

  hydrateNomadJob = { workflowName, stepName, job }: let
    ID = "${workflowName}/${stepName}";
  in
    lib.recursiveUpdate job {
      inherit ID;
      Name = ID;

      TaskGroups = map (
        group: lib.recursiveUpdate group {
          Name = stepName;

          Tasks = map (
            lib.recursiveUpdate {
              Name = stepName;
              Driver = "exec"; # TODO swap out for custom driver
            }
          ) group.Tasks;
        }
      ) job.TaskGroups;
    };

  run = interpreter: with pkgs.writers; let
    binName = "run-workflow-step";
    wrap = env: drv: writeBashBin binName (
      let
        vars = lib.concatStringsSep "\n" (
          lib.mapAttrsToList
            (k: v: "export ${k}=${lib.escapeShellArg v}")
            (removeAttrs env [ "PATH" ])
        );
        path = lib.makeBinPath (
          (with pkgs; [ liftbridge-cli gnutar xz ])
          ++ env.PATH or []
        );
      in
        ''
          ${vars}
          export PATH=${path}
          exec ${drv}/bin/${binName}
        ''
    );
    drv = {
      bash = script: wrap {
        PATH = with pkgs; [ git nodejs ];
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      } (writeBashBin binName script);
      python = script: wrap {} (writePython3Bin binName {} script);
      perl = script: wrap {} (writePerlBin binName {} script);
      js = script: wrap {} (writeJSBin binName {} script);
      haskell = script: wrap {} (writeHaskellBin binName {} script);
      rust = script: wrap {} (writeRustBin binName {} script);
    }.${interpreter};
  in
    script: {
      TaskGroups = [
        {
          Tasks = [
            {
              Config = {
                # TODO this is a derivation, not a flake.
                # waiting for new nomad driver
                flake = drv script;
                command = "/bin/${binName}";
              };
            }
          ];
        }
      ];
    };

  mkStepState =
    { workflowName
    , stepName
    , job
    , inputs
    , when ? {}
    , success ? { ${stepName} = true; }
    , failure ? { ${stepName} = false; }
    }: {
      inherit when inputs success failure;
      job = hydrateNomadJob {
        inherit workflowName stepName;
        job =
          { TaskGroups = []; }
          // lib.optionalAttrs (all lib.id (attrValues when)) job;
      };
    };

  workflow = { name, version ? 0, steps ? {}, meta ? {} }: let
    transformStep = stepName: step: let
      inputNames = attrNames (functionArgs step);
      intersection = lib.intersectLists inputNames (attrNames parsedInputs);
      filteredInputs = lib.listToAttrs (
        map (
          inputName:
            lib.nameValuePair
              inputName
              (parsedInputs.${inputName} or null)
        ) intersection
      );
    in
      mkStepState (
        {
          inherit stepName;
          inputs = inputNames;
          workflowName = name;
        } // (step filteredInputs)
      );

    transformedSteps = lib.mapAttrs transformStep steps;
  in
    {
      inherit name meta version;
      steps = transformedSteps;
    };

  workflows = dir: lib.listToAttrs (
    map (
      file: let
        name = lib.pipe file [
          toString
          (lib.removePrefix "${toString dir}/")
          (lib.removeSuffix ".nix")
        ];
        wf = workflow (
          { inherit name; }
          // (import file) {
            id = toString id;
            inherit run;
          }
        );
      in
        lib.nameValuePair wf.name wf
    ) (lib.filesystem.listFilesRecursive dir)
  );
in

{ workflows = workflows ./workflows; }
