{
  addNomadJobDefaults = job:
    job // {
      datacenters = job.datacenters or [ ]
        ++ [ "dc1" "eu-central-1" "us-east-2" ];
    } // (if !(job ? group) then
      { }
    else {
      group = builtins.mapAttrs (k: group:
        group // (if !(group ? task) then
          { }
        else {
          task = builtins.mapAttrs (k: task:
            task // {
              env = {
                CICERO_WEB_URL = "http://127.0.0.1:8080";
                CICERO_API_URL = "http://127.0.0.1:8080/api";
              } // task.env or { };
            }) group.task;
        })) job.group;
    });
}
