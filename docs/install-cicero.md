# Install Cicero

## How to install Cicero locally

### Clone repository locally

```
git clone https://github.com/input-output-hk/cicero.git
```

### Provide github token with read:org, repo flags

```
cat >> ~/.netrc << EOF
machine github.com
login github_username
password github_token

machine api.github.com
login github_username
password github_token
EOF
```

### Run nix develop, to load Cicero shell

```
nix develop
```

### Start a development instance of Nomad, nomad-follower, Vault and nix-cache-proxy:

```
dev-cluster
```

### Run the required services in Nomad:

```
dev-jobs
```

### Run the application:

```
dev-cicero
```

### Cicero's WebUI should now be available on http://localhost:8080.
