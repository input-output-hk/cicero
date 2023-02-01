# Installing Nix

## 1. Install

Depending on your local environment there are different options to match your needs.

### The Official Way

Consult the following links to install preferrably multi-user mode:

- [Nix download page](https://nixos.org/download.html#nix-install-linux)
- [Nix manual](https://nixos.org/manual/nix/stable/installation/installation.html)

### Community Alternatives

_Note Cicero is not being tested with these._

[Zero to Nix] provides an alternative installer tailored for beginners
and is also a great guide to get started with Nix in general.

You may also try the more lightweight [nix-portable](https://github.com/DavHau/nix-portable)
if you would like to avoid a permanent installation.

## 2. Enable Flakes

If you installed using the [unofficial installer from Determinate Systems](https://zero-to-nix.com/concepts/nix-installer)
that is recommended by [Zero to Nix] you can skip this step as the installer does it for you by default.

Otherwise add the following line to `/etc/nix/nix.conf`:

	experimental-features = nix-command flakes

## 3. Further Learning Resources

If you want to learn more about Nix have a look a the following links:

- community-maintained [wiki](https://nixos.wiki)
	- [Nix expression language](https://nixos.wiki/wiki/Nix_Expression_Language)
- community guides (opinionated)
	- [Zero to Nix]
	- [nix.dev](https://nix.dev)
	- [Nix from first principles](https://tonyfinn.com/blog/nix-from-first-principles-flake-edition)
	- [Nix Flakes: first steps](https://blog.kubukoz.com/flakes-first-steps)
	- [The Nix Way](https://github.com/the-nix-way)
	- [The Nix Hour](https://www.youtube.com/watch?v=wwV1204mCtE&list=PLyzwHTVJlRc8yjlx4VR4LU5A5O44og9in) (YouTube playlist)
	- [How To Learn Nix](https://www.ianthehenry.com/posts/how-to-learn-nix)
	- [Nix Tour](https://nixcloud.io/tour), an interactive tour
- [Nix Pills](https://nixos.org/guides/nix-pills/index.html), a comprehensive semi-officical guide that once was a series of blog posts
- [community-made documentation of library functions](https://teu5us.github.io/nix-lib.html)

[Zero to Nix]: https://zero-to-nix.com
