{ self, id, ... }@args:

let
  inherit (self.lib) std;

  wfLib = import ../../workflows-lib.nix self;

  simple = [ wfLib.jobDefaults std.singleTask ];
in std.callWorkflow args {
  actions = {
    ping = { ping ? null }: {
      when."ping missing" = ping == null;

      job = simple ++ [
        (std.script "bash" ''
          echo running ping ${toString id}
        '')
      ];
    };

    pong = { ping ? null, pong ? null }: {
      when = {
        "ping sent" = ping == true;
        "pong missing" = pong == null;
      };

      job = simple ++ [
        (std.script "bash" ''
          echo running pong ${toString id}
        '')
      ];
    };

    pingpong = { pong ? null, pingpong ? null }: {
      when = {
        "pong sent" = pong == true;
        "pingpong missing" = pingpong == null;
      };

      job = simple ++ [
        (std.script "bash" ''
          echo running pingpong ${toString id}
        '')
      ];
    };
  };
}
