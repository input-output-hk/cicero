let
  inherit (import ./ping.nix) workflow;
in

{ std, lib, actionLib, ... }@args:

std.behavior.onInputChange "state" {
  inputs.state = ''
    "${workflow}": "ping"
  '';

  outputs = _: {
    ${workflow} = "pong";
  };

  job = { state }:
    actionLib.simpleJob args (std.script "bash" ''
      echo 'Received: '${lib.escapeShellArg state}
      echo 'Pong!'
    '');
}
