# Tutorial -- How to write a Cicero Action

## A first visit

After all steps from the [installation](./installation.md) are running a visit to [http://localhost:8080](http://localhost:8080) should show the following UI:


![Cicero WebUI Intro](./cicero_webui_intro.png "Cicero WebUI Intro")

The first visit should also change the url to: [http://localhost:8080/action/current?active](http://localhost:8080/action/current?active)

## Click on "Create New Action"
After clicking the "Create New Action" button the url should change to:
[http://localhost:8080/action/new](http://localhost:8080/action/new)

And the UI should correspond to:
![Cicero WebUI New Action](./cicero_webui_new_action.png "Cicero WebUI New Action")

## Enter the path to the Actions, which should be created
The path to the Actions can be a remote path like [github.com/input-output-hk/cicero](github.com/input-output-hk/cicero)
or a local path like [/home/developer/IOHK/cicero](/home/developer/IOHK/cicero)

**This doesn't need to be a path to Cicero itself:**

Cicero Actions can also be part of a repository, which isn't Cicero.
See [How to include external actions](./how-to-include-external-actions.md) for more information.

### Assume the following path was entered:


![Cicero WebUI New Localpath](./cicero_webui_new_action_localpath.png "Cicero WebUI New Localpath")
