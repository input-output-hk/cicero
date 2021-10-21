{ id, inputs ? {}, inputsJSON ? null }:

let
  inherit (builtins)
    all attrNames attrValues concatStringsSep fromJSON functionArgs getFlake
    hashString seq readDir
    ;

  combinedInputs = inputs // (
    if inputsJSON != null
    then fromJSON inputsJSON
    else {}
  );

  flake = getFlake (toString ./.);
  pkgs =
    flake.inputs.nixpkgs.legacyPackages.x86_64-linux
    // flake.legacyPackages.x86_64-linux;

  inherit (pkgs) lib;

  findRunner = type:
    runners.${type} or (
      throw
        "Invalid step type '${type}'. Available types are: ${
        concatStringsSep ", " (attrNames runners)
        }"
    );

  mkDerivation = { workflowName, stepName, script, ... }@args:
    derivation (
      rec {
        name = lib.strings.sanitizeDerivationName "${workflowName}-${stepName}";
        passAsFile = [ "script" ] ++ args.passAsFile or [];
        inherit script;
        system = "x86_64-linux";
        result = concatStringsSep "." [
          workflowName
          stepName
          (hashString outputHashAlgo script)
        ];
        requiredSystemFeatures = [ "recursive-nix" ];
        outputHashAlgo = "sha256";
        outputHashMode = "flat";
        outputHash = hashString outputHashAlgo result;
        builder = ./builder.sh;
      } // args
    );

  runners = let
    run = ourArgs: stepArgs:
      mkDerivation (stepArgs // ourArgs);
    makeBinPath = extra:
      lib.makeBinPath (with pkgs; [ liftbridge-cli nixUnstable gnutar xz ] ++ extra);
    mkNomadJob = args: {
      Job = lib.recursiveUpdate args.script {
        ID = "${args.workflowName}/${args.stepName}";
        Name = args.stepName;

        TaskGroups = map (
          group: lib.recursiveUpdate group {
            Name = args.stepName;

            Tasks = map (
              lib.recursiveUpdate {
                Name = args.stepName;
                Driver = "exec"; # TODO swap out for custom driver

                Constraints = [
                  {
                    LTarget = "\${meta.run}";
                    Operand = "=";
                    RTarget = "true";
                  }
                ];

                # TODO probably implement somewhere else
                # Meta.run = true|false;
              }
            ) group.Tasks;
          }
        ) args.script.TaskGroups;
      };
    };
  in
    with pkgs; {
      bash = run {
        PATH = makeBinPath [ bash coreutils git ];
        SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
      };
      ruby = run { PATH = makeBinPath [ ruby ]; };
      python = run { PATH = makeBinPath [ python ]; };
      crystal = run { PATH = makeBinPath [ crystal ]; };
      nomad = args: builtins.toJSON (mkNomadJob args);
    };

  mkStep =
    { workflowName
    , stepName
    , inputs
    , job
    , type ? "nomad"
    , when ? {}
    , success ? { ${stepName} = true; }
    , failure ? { ${stepName} = false; }
    }:
      let
        ok = all (a: a) (attrValues when);
        runner = findRunner type;
        drv = runner {
          inherit workflowName stepName type;
          script = job;
        };
      in
        {
          job = if ok then drv else null;
          type = seq runner type;
          inherit when inputs success failure;
        };

  workflow = { name, version ? 0, steps ? {}, meta ? {} }:
    let
      transformStep = stepName: step:
        let
          inputs = attrNames (functionArgs step);
          intersection = lib.intersectLists inputs (attrNames combinedInputs);
          filteredInputs = lib.listToAttrs (
            map (
              input: {
                name = input;
                value = combinedInputs.${input} or null;
              }
            ) intersection
          );
        in
          mkStep (
            {
              workflowName = name;
              inherit stepName inputs;
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
      file: lib.nameValuePair
        (
          lib.pipe file [
            builtins.baseNameOf
            (lib.removeSuffix ".nix")
          ]
        )
        (
          import file {
            id = toString id;
            inherit workflow;
          }
        )
    ) (lib.filesystem.listFilesRecursive dir)
  );
in

{ workflows = workflows ./workflows; }
