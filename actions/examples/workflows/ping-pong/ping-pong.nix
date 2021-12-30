let
  inherit (import ./ping.nix) workflow;
in

{ std, lib, actionLib, ... }@args:

std.behavior.onInputChange "state" args {
  inputs.state = ''
    "${workflow}": "pong"
  '';

  outputs = _: {
    ${workflow} = "ping-pong";
  };

  job = { state }:
    actionLib.simpleJob args (std.script "bash" ''
      echo 'Received: '${lib.escapeShellArg state}
      echo 'Ping Pong!'
    '');
}
