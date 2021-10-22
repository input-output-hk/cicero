{ id, run }:

{
  version = 0;

  steps = {
    ping = { ping ? false }: {
      when."ping missing" = !ping;

      job = (run "bash" ''
        echo running ping ${id}
      '') // (import ../workflows-nomad.nix);
    };

    pong = { ping ? false, pong ? false }: {
      when = {
        "ping sent" = ping;
        "pong missing" = !pong;
      };

      job = (run "bash" ''
        echo running pong ${id}
      '') // (import ../workflows-nomad.nix);
    };

    pingpong = { pong ? false, pingpong ? false }: {
      when = {
        "pong sent" = pong;
        "pingpong missing" = !pingpong;
      };

      job = (run "bash" ''
        echo running pingpong ${id}
      '') // (import ../workflows-nomad.nix);
    };
  };
}
