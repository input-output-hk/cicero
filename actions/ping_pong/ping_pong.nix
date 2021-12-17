let
  common = import ./ping.nix;
in

{ std, lib, ... } @ args:

{
  inputs = lib.recursiveUpdate (common.inputs args) {
    latest = {
      ping = ''
        "${common.workflow}/ping": true
      '';
      pong = ''
        "${common.workflow}/pong": true
      '';
    };
  };

  job = common.job std args;
}