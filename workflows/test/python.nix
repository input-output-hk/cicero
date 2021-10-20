{ id, workflow }:

workflow {
  name = "test/python";

  version = 0;

  steps = {
    python = {}: {
      type = "python";
      job = ''
        print("running python ${id}")
      '';
    };
  };
}
