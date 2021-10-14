{ id, inputs ? { }, inputsJSON ? null }:
let
  inherit (builtins)
    all attrNames attrValues concatStringsSep fromJSON functionArgs getFlake
    hashString seq readDir;

  combinedInputs = inputs
    // (if inputsJSON != null then (fromJSON inputsJSON) else { });

  flake = getFlake (toString ./.);
  pkgs = flake.inputs.nixpkgs.legacyPackages.x86_64-linux
    // flake.legacyPackages.x86_64-linux;
  inherit (pkgs) lib;

  findRunner = type:
    runners.${type} or (throw
      "Invalid task type '${type}'. Available types are: ${
        concatStringsSep ", " (attrNames runners)
      }");

  mkDerivation = { workflowName, taskName, script, ... }@args:
    derivation ((rec {
      name = lib.strings.sanitizeDerivationName "${workflowName}-${taskName}";
      passAsFile = [ "script" ];
      inherit script;
      system = "x86_64-linux";
      result = concatStringsSep "." [
        workflowName
        taskName
        (hashString outputHashAlgo script)
      ];
      outputHashAlgo = "sha256";
      outputHashMode = "flat";
      outputHash = hashString outputHashAlgo result;
      builder = ./builder.sh;
    }) // args);

  runners = with pkgs;
    let
      run = ourArgs: taskArgs: mkDerivation (ourArgs // taskArgs);
      makeBinPath = extra: lib.makeBinPath ([ liftbridge-cli ] ++ extra);
    in {
      bash = run { PATH = makeBinPath [ bash coreutils ]; };
      ruby = run { PATH = makeBinPath [ ruby ]; };
      python = run { PATH = makeBinPath [ python ]; };
      crystal = run { PATH = makeBinPath [ crystal ]; };
    };

  mkTask = { workflowName, taskName, task, inputs, run, type ? "bash"
    , when ? { }, success ? { ${taskName} = true; }
    , failure ? { ${taskName} = false; } }:
    let
      pp = v: builtins.trace (builtins.toJSON v) v;
      ok = all (a: a) (attrValues when);
      runner = findRunner type;
      drv = runner {
        inherit workflowName taskName type;
        script = run;
      };
    in {
      run = if ok then drv else null;
      type = seq runner type;
      inherit when inputs success failure;
    };

  workflow = { name, tasks ? { }, meta ? { } }:
    let
      transformTask = taskName: task:
        let
          inputs = attrNames (functionArgs task);
          intersection =
            pkgs.lib.intersectLists inputs (attrNames combinedInputs);
          filteredInputs = pkgs.lib.listToAttrs (map (input: {
            name = input;
            value = combinedInputs.${input} or null;
          }) intersection);
        in mkTask ({
          workflowName = name;
          inherit taskName task inputs;
        } // (task filteredInputs));
      transformedTasks = pkgs.lib.mapAttrs transformTask tasks;
    in {
      inherit name meta;
      tasks = transformedTasks;
    };

  x = {
    pingpong = { name = "pingpong"; };
    "test/ruby" = { name = "test/ruby"; };
  };

  workflows = dir:
    (lib.mapAttrsToList (name: type:
      if (type == "regular") && (lib.hasSuffix ".nix" name) then
        let
          called = import (dir + "/${name}") {
            id = toString id;
            inherit workflow;
          };
        in [ (pkgs.lib.nameValuePair called.name called) ]
      else if type == "directory" then
        [ (workflows (dir + "/${name}")) ]
      else
        { }) (readDir dir));
in { workflows = lib.listToAttrs (lib.flatten (workflows ./workflows)); }
