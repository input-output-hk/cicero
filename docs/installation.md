# Installation

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

### Which route to go? Actual install cicero as Nixos Module? Or just the development

## How to deploy Cicero remotely
