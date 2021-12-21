rec {
  workflow = "ping_pong";

  inputs = args: {
    start = ''
      "${workflow}/start": true
    '';

    "has run" = {
      not = true;
      match = ''
        "${args.name}": bool
      '';
    };
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
    job = job args;
  };
}
