{
  addNomadJobDefaults = job: job // {
    Datacenters = job.Datacenters or [] ++ [ "dc1" "eu-central-1" "us-east-2" ];
    TaskGroups = map (group: group // {
      Tasks = map (task: task // {
        Env = {
          CICERO_API_URL = "http://127.0.0.1:8080/api";
        } // task.Env or {};
      }) group.Tasks or [];
    }) job.TaskGroups or [];
  };
}
