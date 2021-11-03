{ id, run }:

{
  version = 0;

  actions = {
    python = { }: {
      job = (run "python" ''
        print("running python ${id}")
      '') // (import ../../workflows-nomad.nix);
    };
  };
}
