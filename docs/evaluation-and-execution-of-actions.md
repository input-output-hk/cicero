# Evaluation and Execution of Cicero Actions

## Actions
The [Cicero Base Library](https://github.com/input-output-hk/cicero/blob/main/lib.nix) already provides a good part of specification for Actions:

### An action is a function of the form:
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
output = inputs: {
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

##  Evaluation
Cicero Actions are written in the [nix-expression-language](https://nixos.wiki/wiki/Nix_Expression_Language) plus the actual Cicero Standard Library providing Action specific extensions.

The Cicero Standard Library actually consists of multiple files, which are:
- [Cicero Base Library](https://github.com/input-output-hk/cicero/blob/main/lib.nix)
- [Cicero Extended Library](https://github.com/input-output-hk/cicero/blob/main/pkgs/cicero/evaluators/nix/lib.nix)
- [Cicero Action Library](https://github.com/input-output-hk/cicero/blob/main/action-lib.nix)

In the actual Cicero [flake.nix](https://github.com/input-output-hk/cicero/blob/main/flake.nix), this is all already bundled into a single flake output:
```
...
...
	lib = import ./lib.nix self;

    ciceroActions =
       self.lib.callActionsWithExtraArgs
       rec {
         inherit (self.lib) std;
         inherit (nixpkgs) lib;
         actionLib = import ./action-lib.nix {inherit std lib;};
         nixpkgsRev = nixpkgs.rev;
       }
       ./actions;
...
...
```

The **ciceroActions** flake output is therefore required to let Cicero know that a flakified repository contains executable Actions.

This is also what the Cicero Evaluator is looking for when a new Path is passed to the Cicero WebUI([Tutorial 1](./tutorial-1.md)).

However, there is also cmdline Tool called cicero-evaluator-nix, which just helps running the evaluation part manually([Debuggin Evaluations](./cicero-evaluator-nix.md)).

## Execution

An Action is only invoked and passed to Nomad as new job when a corresponding fact is saved([Tutorial 2](./tutorial-2.md)).

The [fact.go](https://github.com/input-output-hk/cicero/blob/main/src/application/service/fact.go) code saves a fact, which results in InvokeCurrentActive getting called.

This will further result in a JobsRegister call in the [nomad_client.go](https://github.com/input-output-hk/cicero/blob/main/src/application/nomad_client.go)
