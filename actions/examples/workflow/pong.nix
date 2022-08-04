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
      inputs: state: match: "${workflow}": "ping"
      output: success: "${workflow}": "pong"
    '';

    job = {state}:
      actionLib.simpleJob args (std.script "bash" ''
        echo 'Received: '${lib.escapeShellArg state.value.${workflow}}
        echo 'Pong!'
      '');
  }
