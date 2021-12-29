self:

/* An action is a function of the form:

   ```nix
   { name, id }: {
     inputs = {
       # require last fact with `count` of at least 1
       tick = "count: >0";

       # stop at 10 (no `over_nine` input will be created)
       over_nine = {
         not = true;
         match = "count: >9";
       };

       # get all ticks
       ticks = {
         select = "all";
         match = "count: int";
       };

       # has no influence on runnability
       double = {
         optional = true;
         match = "double: true";
       };
     };

     # these are the defaults
     # TODO make it possible to publish no facts at all
     outputs = inputs: {
       success.${name} = true;
       failure.${name} = false;
     };

     job = { tick, ticks, double ? false }:
       # `tick` is the latest fact that matches
       # `ticks` is a list of all facts that match
       # `double` is provided if found
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
    { id, inputs ? throw "no inputs given" }:
    let
      inherit (builtins)
        isFunction typeOf attrValues attrNames functionArgs deepSeq;

      expandAction = {inputs ? {}, outputs ? _: {}, ...}@action: action // {
        inputs = mapAttrs (k: v: {
          select = "latest";
          match = v;
          not = false;
          optional = false;
        } // lib.optionalAttrs (typeOf v != "string") v) inputs;

        outputs = inputs: {
          success.${name} = true;
          failure.${name} = false;
        } // outputs inputs;
      };

      validateAction = { inputs, ... }@action:
        lib.pipe action (map deepSeq [
          (let t = typeOf inputs;
          in if t != "set" then
            throw "`inputs` must be set but is ${t}"
          else
            null)

          (mapAttrs (k: v:
            let t = typeOf v;
            in if t != "string" && t != "set" then
              throw ''
                `inputs."${k}"` must be string or set with keys `select` (optional), `not` (optional), `optional` (optional), and `match` but is ${t}''
            else
              null) inputs)

          (mapAttrs (k: v:
            if typeOf v == "string" then
              null
            else
              let
                select = v.select or "latest";
                t = typeOf select;
              in if t != "string" then
                throw ''`inputs."${k}".select` must be string but is ${t}''
              else if select != "latest" && select != "all" then
                throw ''
                  `inputs."${k}".select` must be either "latest" or "all" but is "${select}"''
              else
                null) inputs)

          (mapAttrs (k: v:
            if typeOf v == "string" then
              null
            else
              let t = typeOf v.not or false;
              in if t != "bool" then
                throw ''`inputs."${k}".not` must be bool but is ${t}''
              else
                null) inputs)

          (mapAttrs (k: v:
            if typeOf v == "string" then
              null
            else
              let t = typeOf v.match;
              in if t != "string" then
                throw ''`inputs."${k}".match` must be string but is ${t}''
              else
                null) inputs)

          (mapAttrs (k: v:
            if v.not && v.optional then
              throw ''
                `inputs."${k}"`.{Not,Optional} are mutually exclusive as both would not make sense''
            else
              null) inputs)

          (map (input:
            if !(inputs ? ${input}) then
              throw ''
                `job` can only take arguments declared in `inputs` which "${input}" is not''
            else
              null) (lib.optionals (action ? job) (attrNames (functionArgs action.job))))
        ]);

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

      mkActionState = action: action // {
          inherit (action) inputs;
          outputs = { inherit (action.outputs inputs) success; };
        } // lib.optionalAttrs (action ? job) {
          outputs = { inherit (action.outputs inputs) success failure; };
          job = hydrateNomadJob (action.job inputs);
        };
    in lib.pipe action [
      (action: if isFunction action then action else import action)
      (action: action { inherit name id; })
      expandAction
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
