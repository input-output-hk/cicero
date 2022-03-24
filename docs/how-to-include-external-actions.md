# How to include external Actions

In this example [lares](https://github.com/input-output-hk/lares) is used, to explain how to setup a external project with Cicero.

First the project needs a [flake.nix](https://github.com/input-output-hk/lares/blob/master/flake.nix) containing **Cicero** itself as flake input and **ciceroActions** as flake output.

## flake.nix:
```nix
{
  description = "Lares";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    cicero = {
      url = "github:input-output-hk/cicero";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix2container.url = "github:nlewo/nix2container";
  };

  outputs = { self, nixpkgs, utils, cicero, nix2container, ... }:
    utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend self.outputs.overlay;
          n2c = nix2container.packages.${system}.nix2container;
        in
        rec {
  	   ...
  	   ...
     }) // rec {
  	   ...
  	   ...

       ciceroActions = cicero.lib.callActionsWithExtraArgs
         rec {
           inherit (cicero.lib) std;
           inherit (nixpkgs) lib;
           actionLib = import "${cicero}/action-lib.nix" { inherit std lib; };
         } ./cicero;
     };
}
```

## Writing the external Action

Now it's required to write the corresponding Action for Cicero to execute later on.

The ciceroActions already hints that it does a recursive lookup in the ./cicero folder, so therefore it's possible to place our Action under the ./cicero/lares directory.

# ci.nix
The Action itself is written as nix expression.

```
// Those are the inputs to the actual Action function
// The path of the file from the starting directory is used as name.
// std, lib & actionLib are provided by the set/rec in the ciceroActions flake output.
{ name, std, lib, actionLib, ... } @ args:


{

  // The inputs.start describes the expected Fact as json input
  // expecting sha & clone_url as fields
  // while statuses_url, ref & default_branch being optional fields
  inputs.start = ''
    "${name}": start: {
      // from both std/ci/{pr,push}
      sha: string
      clone_url: string
      statuses_url?: string

      // only from std/ci/push
      ref?: "refs/heads/\(default_branch)"
      default_branch?: string
    }
  '';

  // The output describes the published state of a Fact
  // A published fact can either be successful or failed
  // This will also be stored in the run_output table in Cicero db
  // after a Run has completed
  output = { start }:
    let cfg = start.value.${name}.start; in
    {
      success.${name} = {
        ok = true;
        revision = cfg.sha;
      } // lib.optionalAttrs (cfg ? ref) {
        inherit (cfg) ref default_branch;
      };
    };

```

