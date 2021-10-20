{ id, workflow }:
workflow {
  version = 0;
  name = "test/python";
  meta.version = "2021.10.14.001";

  steps = {
    python = { }: {
      type = "python";
      job = ''
        print("running python ${id}")
      '';
    };
  };
}
