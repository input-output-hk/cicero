# How to start an Action
The previous chapter explained how to create a Cicero Action.

Now it makes sense explain how to actually start this Action.

## Enter the Cicero development shell

```
nix develop
```

## Use httpie to create a new Fact

By sending a http POST request via httpie to the Cicero-API /api/fact endpoint the Fact gets created or updated.

The creation or update of Facts will trigger the corresponding Actions to run in Cicero.

A fact can have multiple inputs for providing different arguments for calling the actual Action.

But the examples/runners/bash Action doesn't really need any of those inputs for the moment.

Therefore the Fact is empty and the JSON representation is '{}'.

```
http -v POST :8000/api/fact examples/runners/bash:={}
```

## Go to "Runs" tab in Cicero WebUI: [http://localhost:8000/run](http://localhost:8000/run)

This page will provide the exact details how long it took to run the actual Action when it was triggered by the creation of the new fact.

![Cicero WebUI Runs Localpath](./cicero_webui_runs_localpath.png "Cicero WebUI Runs Localpath")

If there is more interest on how the corresponding task was scheduled on Nomad, follow the link under "Nomad Job ID":

![Cicero WebUI Runs Nomad](./cicero_webui_runs_nomad_localpath.png "Cicero WebUI Runs Nomad")
