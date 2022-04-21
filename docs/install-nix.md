# Install Nix

## Prerequisites
- Nix
	- At least version 2.4 >= and < 2.7
- (Linux with systemd)
	- You can run Cicero on other platforms but the [Nix actions library](https://github.com/input-output-hk/cicero/blob/main/pkgs/cicero/evaluators/nix/lib.nix) requires [nomad-driver-nix](https://github.com/input-output-hk/nomad-driver-nix).

## Installation

### Setup MultiUser Nix: [nixos.org/download.html](https://nixos.org/download.html)
#### Linux
```
sh <(curl -L https://nixos.org/nix/install) --daemon
```
#### macOS
```
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume --daemon
```

### Install Flakes
```
nix-env -iA nixpkgs.nixFlakes
```

#### Enable Flakes
```
vim /etc/nix/nix.conf
```

Add the following line:
```
experimental-features = nix-command flakes
```

#### Use nix 2.5
```
nix-env -iA nixpkgs.nix_2_5
nix --version
```

Should return
```
nix (Nix) 2.5.1
```
