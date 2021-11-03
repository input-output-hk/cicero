# Cicero

*Cicero* is a workflow execution engine.  A *workflow* is a description of
(dependent) actions using the *Nix* configuration language. The idea behind
workflows in Cicero is to build reproducible artefacts based on commit hashes
or pull requests. Workflows consist of one more more *Action*. Actions can for
example be source checkouts, build instructions, or quality gates that measure
and ensure certain Key Performance Indicators (KPI) and Key Risk Indicators
(KRI).

*Cicero*’s workflows are flexible enough to build Continuous Integration (CI)
and Continuous Deliver (CD) pipelines. It offers a rich Web UI as well as a CLI
tool for developers to query and inspect workflows and their associated actions,
as well as the action outcomes. Integration with third party applications (e.g.
JIRA) is possible, for automatic status updates. By using a declarative
approach to actions, dependencies and intermediate results can be easily cached,
and execution parallelised, thus reducing build times.

# How To Run

First enter the development shell using either direnv or by running `nix develop`.

Start a development instance of Nomad:

    nomad-dev

Run the required services in Nomad:

    cue export -e jobs.cicero | nomad job run -

Migrate the database:

    dbmate up

Run the application:

    go run . all

# How To …

See the commands listed by:

```
nix develop
```
