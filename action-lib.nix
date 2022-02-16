{ std, lib }:

let
  inherit (std.data-merge) merge;
  inherit (lib) mapAttrs;
in

rec {
  simpleJob = action: job:
    std.chain action [
      (std.escapeNames [ ] [ ])

      std.singleTask

      job
    ];
}
