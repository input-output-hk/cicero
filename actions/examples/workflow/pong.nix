let
  inherit (import ./ping.nix) workflow;
in

{ std, lib, actionLib, ... }@args:

std.behavior.onInputChange "state" workflow args {
  inputs.state = ''
    "${workflow}": "ping"
  '';

  outputs = _: {
    success.${workflow} = "pong";
  };

  job = { state }:
    actionLib.simpleJob args (std.script "bash" ''
      echo 'Received: '${lib.escapeShellArg state.value.${workflow}}
      echo 'Pong!'
    '');
}
