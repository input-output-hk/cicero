# Cicero

*Cicero* is a workflow execution engine.  A *workflow* is a description of (dependent) steps using the *Nix* configuration language. The idea behind workflows in Cicero is to build reproducible artefacts based on commit hashes or pull requests. Workflows consist of one more more *Steps*. Steps can for example be source checkouts, build instructions, or quality gates that measure and ensure certain Key Performance Indicators (KPI) and Key Risk Indicators (KRI).

*Cicero*’s workflows are flexible enough to build Continuous Integration (CI) and Continuous Deliver (CD) pipelines. It offers a rich Web UI as well as a CLI tool for developers to query and inspect workflows and their associated steps, as well as the steps outcomes. Integration with third party applications (e.g. JIRA) is possible, for automatic status updates. By using a declarative approach to steps, dependencies and intermediate results can be easily cached, and execution parallelised, thus reducing build times.

# How To Run

```
nix run github:input-output-hk/bitte#nomad -- agent -dev \
	-config $(nix eval --impure --raw --expr 'builtins.getFlake "github:input-output-hk/nomad-driver-nix" + "/example/agent.hcl"') \
	-plugin-dir $(nix build --no-link --json github:input-output-hk/nomad-driver-nix | jq -r .[].outputs.out)/bin &
nix develop -c foreman start
```

# How To …

See the commands listed by:

```
nix develop
```
