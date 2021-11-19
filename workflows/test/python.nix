{ id, run }:

let lib = import ../../workflows-lib.nix;
in {
  actions = {
    python = { }: {
      job = lib.addNomadJobDefaults (run "python" { } ''
        print("running python ${id}")
      '');
    };
  };
}
