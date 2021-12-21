{ std, lib, ... }:

let
  inherit (std.data-merge) merge;
  inherit (lib) mapAttrs;
in {
  jobDefaults = actions: job:
    merge job (mapAttrs (k: job: {
      datacenters = [ "dc1" "eu-central-1" "us-east-2" ];
      group = mapAttrs (k: group: {
        restart.attempts = 0;
        task = mapAttrs (k: task: {
          env = {
            CICERO_WEB_URL = "http://127.0.0.1:8080";
            CICERO_API_URL = "http://127.0.0.1:8080/api";
          };
        }) group.task or { };
      }) job.group or { };
    }) job);
}
