{ name, std, lib, actionLib, ... } @ args:

# TODO provide std functions for patterns like this
# (a function that creates the entire attrset with `inputs` and `outputs`)

{
  inputs = {
    start = ''
      // arbitrary number to identify this run
      "${name}/start": number
    '';

    "has not run yet" = {
      not = true;
      match = ''
        "${name}": _inputs.start.value."${name}/start"
      '';
    };
  };

  outputs = { start }: {
    success = [ { ${name} = start.value."${name}/start"; } ];
  };

  job = { start }:
    actionLib.simpleJob args (std.script "bash" ''
      echo 'Running once because I have not run with '${lib.escapeShellArg start.value."${name}/start"}' yet.'
    '');
}
