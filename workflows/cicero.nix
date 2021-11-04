{ id, run }:

let
  hasCloneUrl = pr: pr.repository.clone_url or false != false;
  hasGitHash = pr: pr.commit.sha or false != false;

  inherit (import ../workflows-nomad.nix) Datacenters;

  defaultPackages = [
    "github:nixos/nixpkgs/nixpkgs-unstable#coreutils"
    "github:nixos/nixpkgs/nixpkgs-unstable#gnutar"
    "github:nixos/nixpkgs/nixpkgs-unstable#xz"
    "github:nixos/nixpkgs/nixpkgs-unstable#cacert"
    "github:nixos/nixpkgs/nixpkgs-unstable#git"
  ];

  clone = pr: ''
    set -exuo pipefail

    export NIX_CONFIG="experimental-features = nix-command flakes"
    export SSL_CERT_FILE="/current-profile/etc/ssl/certs/ca-bundle.crt"
    export HOME="$PWD/.home"

    mkdir "$HOME"

    git config --global advice.detachedHead false
    git clone --quiet ${pr.repository.clone_url} src
    cd src
    git checkout ${pr.commit.sha}
  '';
in {
  version = 0;

  actions = {
    gocritic = { pr ? { } }: {
      when = {
        "pr.repository.clone_url exists" = hasCloneUrl pr;
        "pr.commit.sha exists" = hasGitHash pr;
      };

      job = run "bash" {
        inherit Datacenters;
        memory = 1024;
        packages = defaultPackages ++ [
          "github:input-output-hk/cicero/69f334ee30ec406bc3a2720b49b7189c2a3b3da1#gocritic"
          "github:input-output-hk/cicero/69f334ee30ec406bc3a2720b49b7189c2a3b3da1#go"
        ];
      } ''
        ${clone pr}

        gocritic check -enableAll ./...
      '';
    };

    nixfmt = { pr ? { } }: {
      when = {
        "pr.repository.clone_url exists" = hasCloneUrl pr;
        "pr.commit.sha exists" = hasGitHash pr;
      };

      job = run "bash" {
        inherit Datacenters;
        memory = 2 * 1024;
        packages = defaultPackages ++ [
          "github:nixos/nixpkgs/nixpkgs-unstable#fd"
          "github:nixos/nixpkgs/nixpkgs-unstable#nixfmt"
        ];
      } ''
        ${clone pr}

        fd -e nix -X nixfmt -c
      '';
    };

    build = { pr ? { }, gocritic ? false, nixfmt ? false }: {
      when = {
        "gocritic passes" = gocritic;
        "nixfmt passes" = nixfmt;
      };

      job = run "bash" {
        inherit Datacenters;
        memory = 2 * 1024;
        cpu = 3000;
        packages = defaultPackages
          ++ [ "github:nixos/nixpkgs/nixpkgs-unstable#nixUnstable" ];
      } ''
        ${clone pr}

        nix-store --load-db < /registration
        nix build
      '';
    };

    deploy = { build ? false, pr ? { } }: {
      when = { "nix build passes" = build; };

      job = run "bash" {
        inherit Datacenters;
        memory = 1024;
        packages = defaultPackages ++ [
          "github:nixos/nixpkgs/nixpkgs-unstable#cue"
          "github:nixos/nixpkgs/nixpkgs-unstable#nomad"
        ];
      } ''
        ${clone pr}

        flake="github:input-output-hk/cicero/${pr.commit.sha}#cicero-entrypoint"
        cue export -e jobs.cicero -t ciceroFlake="$flake" > job.json
        nomad run job.json
      '';
    };
  };
}
