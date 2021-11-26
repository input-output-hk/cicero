{ id, run }:

let
  lib = import ../workflows-lib.nix;

  bash = groupAndTaskName: script:
    lib.addNomadJobDefaults (run "bash" {
      # XXX currently required to show logs in UI
      group = groupAndTaskName;
      task = groupAndTaskName;
    } script);
in {
  actions = {
    ping = { ping ? null }: {
      when."ping missing" = ping == null;

      job = bash "ping" ''
        echo running ping ${id}
      '';
    };

    pong = { ping ? null, pong ? null }: {
      when = {
        "ping sent" = ping == true;
        "pong missing" = pong == null;
      };

      job = bash "pong" ''
        echo running pong ${id}
      '';
    };

    pingpong = { pong ? null, pingpong ? null }: {
      when = {
        "pong sent" = pong == true;
        "pingpong missing" = pingpong == null;
      };

      job = bash "pingpong" ''
        echo running pingpong ${id}
      '';
    };
  };
}
