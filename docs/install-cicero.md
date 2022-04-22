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

### Enter the Cicero development shell
```
nix develop
```

### Start a development VM with Nomad, nomad-follower, Vault and Spongix:
```
nixos-shell --flake .
```

### Inside the VM, run the application:
```
dev-cicero
```

### You can also run it outside the VM if you choose another port:
```
dev-cicero --web-listen :8000
```

### Cicero's WebUI should now be available on http://localhost:8080.
