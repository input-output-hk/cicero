self:

/* A "spec action" is a function of the form:

   ```nix
   { name, id }: {
     inputs = {
       # require last fact with `count` of at least 1
       tick = "count: >0";

       # stop at 10 (no `over_nine` input will be created)
       over_nine = {
         select = "latest_none";
         match = "count: >9";
       };

       # get all ticks
       ticks = {
         select = "all";
         match = "count: int";
       };
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
        isFunction typeOf fromJSON attrValues attrNames functionArgs any;

      parsedInputs = {
        "set" = inputs;
        "string" = fromJSON inputs;
      }.${typeOf inputs};

      validateAction = { inputs, job, ... }@action:
        if typeOf inputs != "set" then
          abort "`inputs` must be an attribute set"
        else if any
        (input: let t = typeOf input; in t != "string" && t != "set")
        (attrValues inputs) then
          abort ''
            values in `inputs` must be string or an attribute set like `{ select = "…"; match = "…"; }`''
        else if any (input: typeOf input.select or "" != "string")
        (attrValues inputs) then
          abort "`inputs.<name>.select` must be a string"
        else if any (input: typeOf input.match or "" != "string")
        (attrValues inputs) then
          abort "`inputs.<name>.match` must be a string"
        else if any (var: !(inputs ? ${var}))
        (attrNames (functionArgs job)) then
          abort "`job` can only take arguments declared in `inputs`"
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

      expandActionInputs = mapAttrs (k: v:
        if typeOf v == "string" then {
          select = "latest";
          match = v;
        } else
          v);

      expandAction = { inputs, success ? [{ ${name} = true; }]
        , failure ? [{ ${name} = false; }], job ? null, }:
        {
          inherit success;
          inputs = expandActionInputs inputs;
        } // lib.optionalAttrs (job != null) {
          inherit failure;
          job = hydrateNomadJob (job parsedInputs);
        };
    in lib.pipe action [
      (action: if isFunction action then action else import action)
      (action: action { inherit name id; })
      validateAction
      expandAction
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
