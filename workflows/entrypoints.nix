{ id, workflow }:

workflow {
  name = "entrypoints";

  version = 0;

  steps = {
    github-pull-requests = { github-pull-requests ? false }: {
      when.once = !github-pull-requests; # TODO remove or depend on nomad "service" task? must always stay running.

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
