{ id, run }:
let bash = script: (run "bash" script) // (import ../workflows-nomad.nix);
in {
  version = 0;

  steps = {
    ping = { ping ? false }: {
      when."ping missing" = !ping;

      job = bash ''
        echo running ping ${id}
      '';
    };

    pong = { ping ? false, pong ? false }: {
      when = {
        "ping sent" = ping;
        "pong missing" = !pong;
      };

      job = bash ''
        echo running pong ${id}
      '';
    };

    pingpong = { pong ? false, pingpong ? false }: {
      when = {
        "pong sent" = pong;
        "pingpong missing" = !pingpong;
      };

      job = bash ''
        echo running pingpong ${id}
      '';
    };
  };
}
