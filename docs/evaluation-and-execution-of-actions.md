# Evaluation and Execution of Cicero Actions

Cicero Actions are written in the [nix-expression-language](https://nixos.wiki/wiki/Nix_Expression_Language) plus the actual [Cicero Standard Library](https://github.com/input-output-hk/cicero/blob/main/lib.nix) providing Action specific extensions.

The Cicero Standard Library actually consists of multiple files, which are:
- [Cicero Base Library](https://github.com/input-output-hk/cicero/blob/main/lib.nix)
- [Cicero Extended Library](https://github.com/input-output-hk/cicero/blob/main/pkgs/cicero/evaluators/nix/lib.nix)
- [Cicero Action Library](https://github.com/input-output-hk/cicero/blob/main/action-lib.nix)

In the actual Cicero [flake.nix](https://github.com/input-output-hk/cicero/blob/main/flake.nix), this is all already bundled into a single flake output:
```
...
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
```

The **ciceroActions** flake output is therefore required to let Cicero know that a flakified repository contains executable Actions.

This is also what the Cicero Evaluator is looking for when a new Path is passed to the Cicero WebUI([Tutorial 1](./tutorial-1.md)).

However, there is also cmdline Tool called cicero-evaluator-nix, which just helps running the evaluation part manually([Debuggin Evaluations](./cicero-evaluator-nix.md)).
