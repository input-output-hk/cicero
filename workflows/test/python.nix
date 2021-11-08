{ id, run }:

{
  actions = {
    python = { }: {
      job = (run "python" {} ''
        print("running python ${id}")
      '') // (import ../../workflows-nomad.nix);
    };
  };
}
