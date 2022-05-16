let
  inherit (import ./ping.nix) workflow;
in
  {
    std,
    lib,
    actionLib,
    ...
  } @ args: {
    io = ''
      inputs: state: match: "${workflow}": "pong"
      output: success: "${workflow}": "ping-pong"
    '';

    job = {state}:
      actionLib.simpleJob args (std.script "bash" ''
        echo 'Received: '${lib.escapeShellArg state.value.${workflow}}
        echo 'Ping Pong!'
      '');
  }
