{ id, workflow }:
workflow {
  name = "pingpong";
  version = 0;

  meta = {
    private = false;
    version = "2021.10.14.001";
  };

  tasks = {
    ping = { ping ? false, pong ? false }: {
      when = {
        "ping missing" = !ping;
        "pong missing" = !pong;
      };

      run = ''
        echo running ping ${id}
      '';
    };

    pong = { ping ? false, pong ? false, coverage ? 0 }: {
      when = {
        "ping sent" = ping;
        "pong missing" = !pong;
        "coverage is over 9000" = coverage > 900;
      };

      run = ''
        echo running pong ${id}
      '';
    };

    pingpong = { ping ? false, pong ? false }: {
      when = {
        "ping sent" = ping;
        "pong sent" = pong;
      };

      run = ''
        echo running pingpong ${id}
      '';
    };
  };
}
