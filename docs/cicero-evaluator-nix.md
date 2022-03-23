# Debugging Evaluations with cicero-evaluator-nix

## Enter the Cicero development shell

```
nix develop
```

## Listing all Actions found in local source repository:

CICERO_ACTION_SRC, needs to point to a repository containing a flake.nix with a ciceroActions flake output.

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

## Evaluating an Action manually

### Evaluating successfully
CICERO_ACTION_NAME, is the actual name of the Action.

CICERO_ACTION_ID, can be arbitrarily or the actual Id from the action table in Cicero's db.

CICERO_ACTION_INPUTS, can be left empty for now.

```
CICERO_ACTION_INPUTS={} CICERO_ACTION_ID=1 CICERO_ACTION_SRC=/home/developer/IOHK/cicero CICERO_ACTION_NAME=examples/runners/bash  cicero-evaluator-nix eval
```

Should output:
```
{}
```

This means the evaluation of the nix code was successfull and the code is correct.

### Producing an expected error

```
echo "hello world" >> /home/developer/IOHK/cicero/actions/examples/runners/bash.nix
```

Running evaluation again:
```
CICERO_ACTION_INPUTS={} CICERO_ACTION_ID=1 CICERO_ACTION_SRC=/home/developer/IOHK/cicero CICERO_ACTION_NAME=examples/runners/bash  cicero-evaluator-nix eval

```

Should output:
```
error: undefined variable 'hello'

       at /nix/store/6wgnisjs3ykan4ps6pr6r9vbaa2yi0m9-source/actions/examples/runners/bash.nix:12:1:

           11| }
           12| hello world
             | ^

```

This means there is an error at line 12, because we appended a "hello world" in there.
