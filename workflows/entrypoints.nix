{ id, run }:

{
  version = 0;

  steps = {
    github-pull-requests = {}: {
      job = {
        Datacenters = [ "eu-west-1" "eu-central-1" "us-east-2" ];

        TaskGroups = [ {
          Name = "prs";

          Tasks = [ {
            Name = "prs";

            Config = {
              flake = "github:input-output-hk/cicero#listen-github";
              command = "/bin/listen-github";
            };
          } ];
        } ];
      };
    };
  };
}
