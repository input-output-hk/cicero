# Prerequisites
- Nix
	- At least version 2.4
	- Enable flakes in your `/etc/nix/nix.conf`: `experimental-features = nix-command flakes`
- (Linux with systemd)
	- You can run Cicero on other platforms but the [Nix actions library](https://github.com/input-output-hk/cicero/blob/main/pkgs/cicero/evaluators/nix/lib.nix) requires [nomad-driver-nix](https://github.com/input-output-hk/nomad-driver-nix).
