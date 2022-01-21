let
  inherit (import ./ping.nix) workflow;
in

{ std, lib, actionLib, ... }@args: {
  inputs.state = ''
    "${workflow}": "ping"
  '';

  output = _: {
    success.${workflow} = "pong";
  };

  job = { state }:
    actionLib.simpleJob args (std.script "bash" ''
      echo 'Received: '${lib.escapeShellArg state.value.${workflow}}
      echo 'Pong!'
    '');
}
