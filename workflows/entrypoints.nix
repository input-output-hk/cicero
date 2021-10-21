{ id, run }:

{
  version = 0;

  steps = {
    github-pull-requests = {}: {
      job = (import ../workflows-nomad.nix) // {
        TaskGroups = [ {
          Name = "prs";

          Tasks = [ {
            Name = "prs";

            Config = {
              # TODO use a NixOS config that has vault access
              flake = "github:input-output-hk/cicero";
              command = "/bin/cicero listen --only github";
            };
          } ];
        } ];
      };
    };
  };
}
