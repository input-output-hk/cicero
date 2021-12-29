rec {
  # Used as prefix common to all actions of this workflow.
  workflow = "ping-pong";

  inputs = args: {
    start = ''
      "${workflow}/start": number
    '';

    "has not run yet" = {
      not = true;
      match = ''
        "${args.name}": _inputs.start.value."${workflow}/start"
      '';
    };
  };

  outputs = args: inputs: {
    success = [
      { ${args.name} = inputs.start.value."${workflow}/start"; }
    ];
  };

  job = { std, name, lib, actionLib, ... }@args: _:
    actionLib.simpleJob args (std.script "bash" ''
      echo 'running '${lib.escapeShellArg name}
    '');

  __functor = _: args: {
    inputs = inputs args;
    outputs = outputs args;
    job = job args;
  };
}
