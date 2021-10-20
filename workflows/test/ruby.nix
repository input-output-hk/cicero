{ id, workflow }:
workflow {
  version = 0;
  name = "ruby";
  meta.version = "2021.10.14.001";

  steps = {
    ruby = { }: {
      type = "ruby";

      job = ''
        puts "running ruby ${id}"
      '';
    };
  };
}
