# Install Nix

Depending on your local environment there are different options to match your needs.

Consult the following links to install preferrably multi-user mode:

- [Nix download page](https://nixos.org/download.html#nix-install-linux)
- [Nix manual](https://nixos.org/manual/nix/stable/installation/installation.html)

You may also try the more lightweight [nix-portable](https://github.com/DavHau/nix-portable).

## Enable flakes

Add the following line to `/etc/nix/nix.conf`:

	experimental-features = nix-command flakes
