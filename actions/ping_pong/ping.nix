rec {
  workflow = "ping_pong";

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

  job = { std, name, id, ... }@args: let
    wfLib = import ../../workflows-lib.nix args;
  in inputs: std.chain args [
    wfLib.jobDefaults

    # systemd-nspawn does not like underscore
    (std.escapeNames [ "_" ] [ "-" ])

    std.singleTask

    (std.script "bash" ''
      echo 'running ${name} #${id}'
    '')
  ];

  __functor = _: args: {
    inputs = inputs args;
    outputs = outputs args;
    job = job args;
  };
}
