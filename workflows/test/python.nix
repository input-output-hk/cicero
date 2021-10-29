{ id, run }:

{
  version = 0;

  steps = {
    python = { }: {
      job = (run "python" ''
        print("running python ${id}")
      '') // (import ../../workflows-nomad.nix);
    };
  };
}
