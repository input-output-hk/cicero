self:
/*
  An action is a function of the form:
 
 ```nix
 { name, id }: {
 io = ''
   inputs: {
     # require last fact with `count` of at least 1
     tick: match: count: >0
 
     # stop at 10 (no `over_nine` input will be created)
     over_nine: {
       not: true
       match: count: >9
     }
 
     # has no influence on runnability
     double: {
       optional: true
       match: double: true
     }
   }
 
   output: {
     # this is the default
     success: null
 
     failure: count: inputs.tick.value.count
   }
 '';
 
 job = { tick, double ? false }:
 # `tick` is the latest fact that matches
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
  callAction = name: action: {
    id,
    inputs ? throw "no inputs given",
    ociRegistry ? null,
  }: let
    inherit
      (builtins)
      isFunction
      typeOf
      deepSeq
      ;

    validateAction = {io, ...} @ action:
      lib.pipe action (map deepSeq [
        (
          let
            t = typeOf io;
          in
            if t != "string"
            then throw "`io` must be string but is ${t}"
            else null
        )
      ]);

    hydrateNomadJob = mapAttrs (k: job:
      assert !job ? ID; # author must not set an ID
      
        lib.recursiveUpdate job ({
            type = "batch";
          }
          // lib.optionalAttrs (job ? group) {
            group =
              mapAttrs
              (k: group:
                lib.recursiveUpdate group (lib.optionalAttrs (group ? task) {
                  task =
                    mapAttrs (k: lib.recursiveUpdate {driver = "nix";})
                    group.task;
                }))
              job.group;
          }));

    mkActionState = action:
      action
      // {
        inherit (action) io;
      }
      // lib.optionalAttrs (action ? job) {
        job = hydrateNomadJob (action.job inputs);
      };
  in
    lib.pipe action [
      (action:
        if isFunction action
        then action
        else import action)
      (action: action {inherit name id ociRegistry;})
      validateAction
      mkActionState
    ];

  # Recurses through a directory, considering every file an action.
  # The path of the file from the starting directory is used as name.
  listActions = dir:
    lib.listToAttrs (map
      (file:
        lib.nameValuePair
        (lib.pipe file [
          toString
          (lib.removePrefix "${toString dir}/")
          (lib.removeSuffix ".nix")
        ])
        file)
      (lib.filesystem.listFilesRecursive dir));

  # Like `listActions` but calls every action.
  callActions = dir:
    mapAttrs callAction (listActions dir);

  callActionsWithDefaults = defaults: dir:
    mapAttrs (k: actionWithDefaults defaults) (callActions dir);

  callActionsWithExtraArgs = extras: dir:
    mapAttrs (k: v: callAction k (actionWithExtraArgs extras (import v))) (listActions dir);

  actionWithDefaults = defaults: innerAction: args:
    defaults
    // mapAttrs (k: v:
      if v == null
      then defaults.${k} or null
      else v) (innerAction args);

  actionWithExtraArgs = extras: innerAction: args: innerAction (args // extras);

  std = import ./pkgs/cicero/evaluators/nix/lib.nix self;
}
