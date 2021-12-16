self:

/* A "spec action" is a function of the form:

   ```nix
   { name, id }: {
     inputs = {
       # require last fact with `count` of at least 1
       last.tick = "count: >0";

       # stop at 10 (no `over_nine` input will be created)
       last_none.over_nine = "count: >9";

       # get all ticks
       all.ticks = "count: int";

       # TODO change input def to this:
       # tick = {
       #   select = "latest";
       #   cue = "count: int";
       # };
       # tick = "count: int"; # defaults to select=latest
     };

     job = { tick, ticks }:
       # `tick` is the latest fact that matches
       # `ticks` is a list of all facts that match
       …; # nomad HCL job spec in JSON format
   }
   ```
*/

let
  inherit (self.inputs.nixpkgs) lib;
  inherit (builtins) mapAttrs;
in rec {
  # Calls an action or a file containing one.
  callAction = name: action:
    { id, inputs ? { } }:
    let
      inherit (builtins)
        isFunction typeOf fromJSON attrValues attrNames functionArgs all any;

      parsedInputs = {
        "set" = inputs;
        "string" = fromJSON inputs;
      }.${typeOf inputs};

      validateAction = { inputs, job, ... }@action:
        if !(typeOf inputs == "set") then
          abort "`inputs` must be an attribute set"
        else if !(typeOf (inputs.latest or { }) == "set") then
          abort "`inputs.latest` must be an attribute set"
        else if !(typeOf (inputs.latest_none or { }) == "set") then
          abort "`inputs.latest_none` must be an attribute set"
        else if !(typeOf (inputs.all or { }) == "set") then
          abort "`inputs.all` must be an attribute set"
        else if !(all (var: typeOf var == "string")
          (attrValues (inputs.latest or { }))) then
          abort "`inputs.latest` must only contain strings"
        else if !(all (var: typeOf var == "string")
          (attrValues (inputs.latest_none or { }))) then
          abort "`inputs.latest_none` must only contain strings"
        else if !(all (var: typeOf var == "string")
          (attrValues (inputs.all or { }))) then
          abort "`inputs.all` must only contain strings"
        else if !(all
          (var: (inputs.latest or { }) ? ${var} || (inputs.all or { }) ? ${var})
          (attrNames (functionArgs job))) then
          abort
          "`job` can only take arguments declared in `inputs.{latest,latest_none,all}`"
        else if any (var: (inputs.latest or { }) ? ${var})
        (attrNames (inputs.all or { }))
        || any (var: (inputs.all or { }) ? ${var})
        (attrNames (inputs.latest or { })) then
          abort
          "There must be no duplicate input declarations in `inputs.{latest,all}`"
        else
          action;

      hydrateNomadJob = mapAttrs (k: job:
        assert !(job ? ID); # author must not set an ID
        lib.recursiveUpdate job ({
          type = "batch";
        } // lib.optionalAttrs (job ? group) {
          group = mapAttrs (k: group:
            lib.recursiveUpdate group (lib.optionalAttrs (group ? task) {
              task = mapAttrs (k: lib.recursiveUpdate { driver = "nix"; })
                group.task;
            })) job.group;
        }));

      mkActionState = { inputs, success ? [{ ${name} = true; }]
        , failure ? [{ ${name} = false; }], job ? null, }:
        {
          inherit inputs success;
        } // lib.optionalAttrs (job != null) {
          inherit failure;
          job = hydrateNomadJob { ${name} = job parsedInputs; };
        };
    in lib.pipe action [
      (action: if isFunction action then action else import action)
      (action: action { inherit name id; })
      validateAction
      mkActionState
    ];

  # Recurses through a directory, considering every file an action.
  # The path of the file from the starting directory is used as name.
  listActions = dir:
    lib.listToAttrs (map (file:
      lib.nameValuePair (lib.pipe file [
        toString
        (lib.removePrefix "${toString dir}/")
        (lib.removeSuffix ".nix")
      ]) file) (lib.filesystem.listFilesRecursive dir));

  # Like `listActions` but calls every action.
  callActions = dir: mapAttrs callAction (listActions dir);

  callActionsWithDefaults = defaults: dir:
    mapAttrs (k: actionWithDefaults defaults) (callActions dir);

  callActionsWithExtraArgs = extras: dir:
    mapAttrs (k: v: callAction k (actionWithExtraArgs extras (import v)))
    (listActions dir);

  actionWithDefaults = defaults: innerAction: args:
    defaults // mapAttrs (k: v: if v == null then defaults.${k} or null else v)
    (innerAction args);

  actionWithExtraArgs = extras: innerAction: args: innerAction (args // extras);

  std = import ./pkgs/cicero/evaluators/nix/lib.nix self;
}
