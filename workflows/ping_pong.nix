{ id, workflow }:
workflow {
  name = "pingpong";
  version = 0;

  meta = {
    private = false;
    version = "2021.10.14.001";
  };

  tasks = {
    ping = { ping ? false }: {
      when = { "ping missing" = !ping; };

      run = ''
        echo running ping ${id}
      '';
    };

    pong = { ping ? false, pong ? false }: {
      when = {
        "ping sent" = ping;
        "pong missing" = !pong;
      };

      run = ''
        echo running pong ${id}
      '';
    };

    pingpong = { pong ? false, pingpong ? false }: {
      when = {
        "pong sent" = pong;
        "pingpong missing" = !pingpong;
      };

      run = ''
        echo running pingpong ${id}
      '';
    };
  };
}
