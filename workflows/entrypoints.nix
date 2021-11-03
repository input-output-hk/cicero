{ id, run }:

{
  version = 0;

  actions = {
    github-pull-requests = { }: {
      job = (import ../workflows-nomad.nix) // {
        TaskGroups = [{
          Name = "prs";

          Tasks = [{
            Name = "prs";

            Config = {
              flake = "github:input-output-hk/cicero#listen-github";
              command = "/bin/listen-github";
            };
          }];
        }];
      };
    };
  };
}
