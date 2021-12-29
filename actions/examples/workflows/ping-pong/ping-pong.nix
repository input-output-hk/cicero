let
  common = import ./ping.nix;
in

args:

{
  inputs = common.inputs args // {
    ping = ''
      "${common.workflow}/ping": true
    '';
    pong = ''
      "${common.workflow}/pong": true
    '';
  };

  job = common.job args;
}
