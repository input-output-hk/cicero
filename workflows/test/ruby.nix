{ id, workflow }:

workflow {
  name = "ruby";

  version = 0;

  steps = {
    ruby = {}: {
      type = "ruby";

      job = ''
        puts "running ruby ${id}"
      '';
    };
  };
}
