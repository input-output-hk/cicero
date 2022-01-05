{ std, lib }:

let
  inherit (std.data-merge) merge;
  inherit (lib) mapAttrs;
in

rec {
  jobDefaults = args: job:
    merge job (mapAttrs
      (k: job: {
        datacenters = [ "dc1" "eu-central-1" "us-east-2" ];
        group = mapAttrs
          (k: group: {
            restart.attempts = 0;
            task = mapAttrs
              (k: task: {
                env = {
                  CICERO_WEB_URL = "http://127.0.0.1:8080";
                  CICERO_API_URL = "http://127.0.0.1:8080/api";
                };
              }) group.task or { };
          }) job.group or { };
      })
      job);

  simpleJob = action: job:
    std.chain action [
      jobDefaults

      # systemd-nspawn does not like underscore
      (std.escapeNames [ "_" ] [ "-" ])

      std.singleTask

      job
    ];
}
