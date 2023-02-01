# Nix Evaluator

The Nix evaluator reads actions from a Nix [flake](https://nixos.wiki/wiki/Flakes).
Nix with flake support must be installed.

It expects a flake output called `cicero` with a key for every supported system under it,
just like, for example, the usual `packages` output.

Each system namespace is an attribute set of actions.

For example:

	.#cicero.x86_64-linux.foo

An action is a function that returns an attribute set consisting of [`meta`](action.md#meta), [`io`](action.md#io), [`job`](action.md#job), and [`prepare`](#preparation-hooks).
As its only argument the function takes an attribute set with the following keys:

| Key           | Description                                                                                                                                                                                                                                     |
|---------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `id`          | The UUID of the action being evaluated.                                                                                                                                                                                                         |
| `inputs`      | The [`inputs`](evaluator.md#environment-variables) passed to the Nix evaluator unchanged.                                                                                                                                                       |
| `ociRegistry` | (optional) The OCI registry to encode in the image name. This is only used for the [`nix2container` preparation hook](#nix2container). Might be deleted again, see [nix2container issue #61](https://github.com/nlewo/nix2container/issues/61). |

## Preparation Hooks

An action may define none, one, or multiple hooks in a `prepare` key.

Those are used to prepare some external state needed by the job
and are run after evaluation when Cicero [requests](evaluator.md#command-line-arguments) the `job` attribute.

The value of the `prepare` key must be a list of attribute sets,
each of which elements is an object with a `type` key and possibly others, depending on the type.

There are currently only two hooks implemented.

### `nix2container`

This builds an image using [`nix2container`](https://github.com/nlewo/nix2container) and pushes it to a registry.

| Field Name | Description                                                                                             |
|------------|---------------------------------------------------------------------------------------------------------|
| `type`     | `nix2container`                                                                                         |
| `imageDrv` | Path to `.drv` file of the image. Must exist in the local Nix store after evaluation.                   |
| `name`     | The registry URL to push to, such as `docker://registry.ci.iog.io/ci:3lgzx79gf7m2shx3ls5dwd09kpwsibpp`. |

### `nix`

This builds Nix packages.

By configuring a [post-build hook](https://nixos.org/manual/nix/stable/advanced-topics/post-build-hook.html) you can use this to push them to a binary cache.

| Field Name    | Description                                                                                  |
|---------------|----------------------------------------------------------------------------------------------|
| `type`        | `nix`                                                                                        |
| `derivations` | An array of paths to `.drv` files. These must exist in the local Nix store after evaluation. |

## Debugging

You can run the Nix evaluator manually to try it our or debug your source.
It is available as a flake output at `github:input-output-hk/cicero#cicero-evaluator-nix`.

Let's do some basic operations in Cicero's repository.

### Listing Actions Available in the Repository

```sh
$ cicero-evaluator-nix list
Getting current system…
Got current system: x86_64-linux
Evaluating…
{"event":"result","result":["cicero/ci"]}
```

The log messages are printed on stderr and not part of the protocol.

### Evaluating an Action

```sh
$ CICERO_ACTION_ID=foo \
  CICERO_ACTION_NAME=cicero/ci \
  CICERO_ACTION_INPUTS='{
    "GitHub Push or PR": {
      "id": "bar",
      "created_at": "2023-02-01T14:29:28,251197177+01:00",
      "binary_hash": null,
      "value": {
        "baz": "qux"
      }
    }
  }' \
  cicero-evaluator-nix eval meta io job prepare
Getting current system…
Got current system: x86_64-linux
Evaluating variables…
Evaluating…
{"event":"result","result":{"io":…,"job":…,"prepare":[{"derivations":[…],"type":"nix"}]}}
Preparing nix…
Pushing to http://127.0.0.1:17745?compression=none: …
```

Again, only the event is printed on stdout and part of the protocol.
It is shortened here for brevity.
