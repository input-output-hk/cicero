# Cicero

*Cicero* is a workflow execution engine.  A *workflow* is a description of (dependent) tasks using the *nix* configuration language. The idea behind workflows in cicero is to build reproducible artefacts based on commit hashes or pull requests. Workflows consist of one more more *Tasks*.  Tasks can for example be source checkouts, build instructions, or quality gates that measure and ensure certain Key Performance Indicators (KPI) and Key Risk Indicators (KRI).

*Cicero*’s workflows are flexible enough to build Continuous Integration (CI) and Continuous Deliver (CD) pipelines. It offers a rich Web UI as well as a CLI tool for developers to query and inspect workflows and their associated tasks, as well as the tasks outcomes. Integration with third party applications (e.g. JIRA) is possible, for automatic status updates. By using a declarative approach to tasks, dependencies and intermediate results can be easily cached, and execution parallelised, thus reducing build times.

# How To Start A Development Environment

```
nix develop

foreman run liftbridge &

# restart on changes to source files
nix-shell -p ripgrep entr
rg --files | entr -r go run . all
```
