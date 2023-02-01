# Running Cicero locally

Cicero requires a running [Nomad](https://www.nomadproject.io) cluster to schedule jobs on.
Logs are shipped to [Loki](https://grafana.com/oss/loki) by [nomad-follower](https://github.com/input-output-hk/nomad-follower).
Additionally [Vault](https://www.vaultproject.io) is used to provision secrets in these jobs,
[Spongix](https://github.com/input-output-hk/spongix) provides a Nix binary cache and we are running a docker registry for podman jobs.

To spare you from installing all of these components separately and for quicker development iterations
we provide a QEMU VM using [nixos-shell](https://github.com/Mic92/nixos-shell). It spins up all services
and reads your `~/.netrc` as well as `/etc/nix/netrc` to provide jobs with your credentials.

## 1. Set up your netrc file

You likely already have a `~/.netrc` file.
If you do not it would be a good time to create one now.

To authenticate with GitHub you will need a [token](https://github.com/settings/tokens) with `read:org` and `repo` permissions.

```sh
cat >> ~/.netrc <<EOF
machine github.com
login github_username
password github_token

machine api.github.com
login github_username
password github_token
EOF
```

## 2. Clone the Cicero repository

	git clone https://github.com/input-output-hk/cicero

## 3. Enter the Cicero development shell

This provides useful tools and sets up some environment variables.

If you have [direnv](https://direnv.net) installed it will attempt to load the dev shell automatically.
You may need to run `direnv allow` if this is your first time doing this.

Otherwise enter the dev shell manually:

	nix develop

## 4. Run the VM

	nixos-shell --flake .

It may take a few minutes to build the machine image and start up.
Then a login prompt should appear:

![dev VM login](./cicero_qemu_prompt.png)

Log in as `root` with no password.

You will automatically be thrown into the dev shell in the VM as well.

## 5. Run Cicero

You can run Cicero inside or outside the VM.
During development it is nicer to run it outside as that will be a bit faster.

### Running inside the VM

In the VM shell, run:

	dev-cicero

Cicero's web UI should now be available on [http://localhost:18080](http://localhost:18080).

### Running outside the VM

Open another terminal and run:

	dev-cicero --web-listen :8000

Chosing another port is necessary as the default port 18080 is already bound by the running VM.

Cicero's web UI should now be available on [http://localhost:8000](http://localhost:8000).
