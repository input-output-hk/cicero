{ id, run }:

let bash = script: (run "bash" {} script) // (import ../workflows-nomad.nix);

in {
  version = 0;

  actions = {
    ping = { ping ? null }: {
      when."ping missing" = ping == null;

      job = bash ''
        echo running ping ${id}
      '';
    };

    pong = { ping ? null, pong ? null }: {
      when = {
        "ping sent" = ping == true;
        "pong missing" = pong == null;
      };

      job = bash ''
        echo running pong ${id}
      '';
    };

    pingpong = { pong ? null, pingpong ? null }: {
      when = {
        "pong sent" = pong == true;
        "pingpong missing" = pingpong == null;
      };

      job = bash ''
        echo running pingpong ${id}
      '';
    };
  };
}
