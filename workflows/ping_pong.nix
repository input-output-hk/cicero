{ id, workflow }:
workflow {
  name = "pingpong";

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

    pong = { ping ? false, pong ? false }: {
      when = {
        "ping sent" = ping;
        "pong missing" = !pong;
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

