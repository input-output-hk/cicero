{ id, workflow }:

workflow {
  name = "entrypoints";

  version = 0;

  tasks = {
    github-pull-requests = {}: {
      type = "nomad";

      run = {
        Namespace = "cicero";
        Datacenters = [ "eu-west-1" "eu-central-1" "us-east-2" ];

        TaskGroups.prs.Tasks.prs = {
          Driver = "cicero";

          Config = {
            flake_deps = [ "github:input-output-hk/cicero#listen-github" ];
            command = "/bin/listen-github";
          };

          Constraint = {
            LTarget = "\${meta.run}";
            Operand = "=";
            RTarget = "true";
          };
        };
      };
    };
  };
}
