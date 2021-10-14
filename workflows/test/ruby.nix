{ id, workflow }:
workflow {
  name = "ruby";
  meta.version = "2021.10.14.001";

  tasks = {
    ruby = { }: {
      type = "ruby";

      run = ''
        puts "running ruby ${id}"
      '';
    };
  };
}
