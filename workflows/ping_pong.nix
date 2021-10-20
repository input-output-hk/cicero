{ id, workflow }:

workflow {
  name = "pingpong";

  version = 0;

  meta = {
    private = false;
    version = "2021.10.14.001";
  };

  steps = {
    ping = { ping ? false }: {
      when = { "ping missing" = !ping; };

      type = "bash";

      job = ''
        echo running ping ${id}
      '';
    };

    pong = { ping ? false, pong ? false }: {
      when = {
        "ping sent" = ping;
        "pong missing" = !pong;
      };

      type = "bash";

      job = ''
        echo running pong ${id}
      '';
    };

    pingpong = { pong ? false, pingpong ? false }: {
      when = {
        "pong sent" = pong;
        "pingpong missing" = !pingpong;
      };

      type = "bash";

      job = ''
        echo running pingpong ${id}
      '';
    };
  };
}
