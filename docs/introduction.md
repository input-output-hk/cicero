# Cicero Handbook

Cicero is an action execution engine for running Continuous Integration (CI) and Continuous Delivery (CD) tasks.

In contrast to CI/CD tools like [Jenkins](https://www.jenkins.io/)
or [Hydra](https://github.com/NixOS/hydra) Cicero provides more flexibility in writing pipelines
and a deep integration of the [Nix Expression Language](https://nixos.wiki/wiki/Nix_Expression_Language) and the [Nix Package Manager](https://nixos.wiki/wiki/Nix_Package_Manager).

Cicero also offers a rich Web UI to query and inspect **actions** and their **runs**,
as well as the **action output**.

To interact with the Cicero-API via CLI, HTTP requests are sent to the api endpoints.
The api definition can be found here: [https://cicero.infra.aws.iohkdev.io/documentation/cicero.yaml](https://cicero.infra.aws.iohkdev.io/documentation/cicero.yaml).

**Actions** are defined in the [Nix Expression Language](https://nixos.wiki/wiki/Nix_Expression_Language) which is extended by Cicero specific libraries.
Therefore the execution of an **action** is called **run**.

**Runs** are converted into a job description which gets deployed to a Hashicorp Nomad instance.

Integration with third party applications (e.g. JIRA) is possible, for automatic status updates.

By using a declarative approach to **actions**, dependencies and intermediate results can be easily cached,
and execution parallelised, thus reducing build times.

Vault from the Hashicorp tech stack is used to enable authentication.

The [Cicero Readme](https://github.com/input-output-hk/cicero) provides more detail.

## Topics
- [Installation](./installation.md)
  - [Install Nix](./install-nix.md)
  - [Install Cicero](./install-cicero.md)
- [Tutorial(deprecated)](./tutorial.md)
  - [How to create an Action](./tutorial-1.md)
  - [How to start an Action](./tutorial-2.md)
  - [How to include external Actions](./tutorial-3.md)
- [Tullia tutorial](./tullia.md)
- [Learn Nix](./learn-nix.md)
  - [Evaluation and Execution of Cicero Actions](./evaluation-and-execution-of-actions.md)
  - [Debugging Evaluations with cicero-evaluator-nix ](./cicero-evaluator-nix.md)
  - [Facts explained](./facts-explained.md)
