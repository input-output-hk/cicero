# Debugging Evaluations with cicero-evaluator-nix

## Enter the Cicero development shell

```
nix develop
```

## Listing all Actions found in local source:

```
CICERO_ACTION_SRC=/home/developer/IOHK/cicero cicero-evaluator-nix list
```

Should output:
```
["cicero/ci/lintAndBuild","cicero/ci/schemathesis","cicero/webhooks",
"examples/behaviors/onInputChange","examples/behaviors/onUpdate","examples/post-fact",
"examples/runners/bash","examples/runners/js","examples/runners/perl",
"examples/runners/python","examples/workflow/ping","examples/workflow/ping-pong",
"examples/workflow/pong","std/ci/pr","std/ci/push"]
```
