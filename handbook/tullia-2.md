# Input description(actions.nix) explained

The [input description](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/actions.nix) is written in [nix](https://nixos.wiki/wiki/Nix_Expression_Language) and [cuelang](https://cuelang.org/).

## Example from [tullia-example/rust/tullia/actions.nix](https://github.com/input-output-hk/tullia-example/blob/main/rust/tullia/actions.nix):
```nix
{
  # "rust/build" is the action's name
  "rust/build" = {

    # task, referes to a corresponding task in tullia/tasks.nix
    # this task is executed if the requirements of the input description are fulfilled
    task = "build";

    # io, contains the actual input description in cuelang
    # The module.nix in Tullia merges the cue in io with tullia's cue-libs(lib/*.cue)
    # into the action..io attribute
    # Cicero uses the resulting cue configuration to match & verify incoming inputs(as json)
    # for a specific action
    # module.nix: https://github.com/input-output-hk/tullia/blob/main/nix/module.nix#L994-L1010
    # action..io: https://github.com/input-output-hk/tullia/blob/main/doc/src/module.md#actionio--path
    io = ''

      # github, is a set of repository and input information
      # this set is reused to subscribe to multiple inputs
      let github = {
        #input: "GitHub event"
        #repo: "input-output-hk/tullia-example"
      }

      # lib.merge, ios and lib.io.github*, allow the user
      # to describe inputs & output with less overhead
      # merge & ios: https://github.com/input-output-hk/tullia/blob/main/lib/prelude.cue
      # lib.io.github: https://github.com/input-output-hk/tullia/blob/main/lib/github.cue
      #lib.merge
      #ios: [

        # The input/output entries in ios will tell Cicero to only run the 'build' task
        # if a github_push or github_pr event for the 'tullia-example' repository happens
        #lib.io.github_push & github,
        #lib.io.github_pr   & github,
      ]
    '';
  };
}
```
