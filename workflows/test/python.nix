{ id, workflow }:
workflow {
  name = "test/python";
  meta.version = "2021.10.14.001";

  tasks = {
    python = { }: {
      type = "python";
      run = ''
        print("running python ${id}")
      '';
    };
  };
}
