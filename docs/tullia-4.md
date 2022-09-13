# Creating an Action in the Cicero-WebUI

## A first visit

After all steps from the [installation](./installation.md) a visit to [http://localhost:8000](http://localhost:8000) should show the following UI:


![Cicero WebUI Intro](./cicero_webui_intro.png "Cicero WebUI Intro")

The first visit should also change the url to: [http://localhost:8000/action/current?active](http://localhost:8000/action/current?active)

## Click on "Create New Action"
After clicking the "Create New Action" button the url should change to:
[http://localhost:8000/action/new](http://localhost:8000/action/new)

And the UI should correspond to:
![Cicero WebUI New Action](./cicero_webui_new_action.png "Cicero WebUI New Action")

### Enter the path to the Actions, which should be created

**For any url the 'https://' part needs to be omitted.**

The path to the Actions can be a remote path like [github.com/input-output-hk/tullia-example](github.com/input-output-hk/tullia-example)
or a local path like [/home/developer/IOHK/tullia-example](/home/developer/IOHK/tullia-example/).

**This doesn't need to be a path to Cicero itself:**

Cicero Actions can also be part of a repository, which isn't Cicero.
Projects are required to have a flake.nix with **ciceroActions** as flake output to work with Cicero.
