{ id, run }:

let
  hasCloneUrl = pr: pr.repository.clone_url or false != false;
  hasGitHash = pr: pr.commit.sha or false != false;

  lib = import ../workflows-lib.nix;

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
  actions = {
    gocritic = { pr ? { }, gocritic ? null }: {
      when = {
        "pr.repository.clone_url exists" = hasCloneUrl pr;
        "pr.commit.sha exists" = hasGitHash pr;
        "gocritic hasn't run yet" = gocritic == null;
      };

      job = lib.addNomadJobDefaults (run "bash" {
        memory = 1024;
        packages = defaultPackages ++ [
          "github:input-output-hk/cicero/69f334ee30ec406bc3a2720b49b7189c2a3b3da1#gocritic"
          "github:input-output-hk/cicero/69f334ee30ec406bc3a2720b49b7189c2a3b3da1#go"
        ];
      } ''
        ${clone pr}

        gocritic check -enableAll ./...
      '');
    };

    nixfmt = { pr ? { }, nixfmt ? null }: {
      when = {
        "pr.repository.clone_url exists" = hasCloneUrl pr;
        "pr.commit.sha exists" = hasGitHash pr;
        "nixfmt hasn't run yet" = nixfmt == null;
      };

      job = lib.addNomadJobDefaults (run "bash" {
        memory = 2 * 1024;
        packages = defaultPackages ++ [
          "github:nixos/nixpkgs/nixpkgs-unstable#fd"
          "github:nixos/nixpkgs/nixpkgs-unstable#nixfmt"
        ];
      } ''
        ${clone pr}

        fd -e nix -X nixfmt -c
      '');
    };

    build = { pr ? { }, gocritic ? null, nixfmt ? null, build ? null }: {
      when = {
        "gocritic passes" = gocritic == true;
        "nixfmt passes" = nixfmt == true;
        "build hasn't run yet" = build == null;
      };

      job = lib.addNomadJobDefaults (run "bash" {
        memory = 4 * 1024;
        cpu = 16000;
        packages = defaultPackages
          ++ [ "github:nixos/nixpkgs/nixpkgs-unstable#nixUnstable" ];
      } ''
        ${clone pr}

        mkdir -p /etc
        echo 'nixbld:x:30000:nixbld1' > /etc/group
        echo 'nixbld1:x:30001:30000:Nix build user 1:/var/empty:/bin/nologin' > /etc/passwd
        echo "nameserver ''${NAMESERVER:-1.1.1.1}" > /etc/resolv.conf
        nix-store --load-db < /registration
        nix build
      '');
    };

    deploy = { pr ? { }, environment ? null, gocritic ? null, nixfmt ? null
      , build ? null, deploy ? null }: {
        when = {
          "build passes" = build == true;
          "deploy hasn't run yet" = deploy == null;
        };

        job = lib.addNomadJobDefaults (run "bash" {
          memory = 1024;
          packages = defaultPackages ++ [
            "github:nixos/nixpkgs/nixpkgs-unstable#cue"
            "github:nixos/nixpkgs/nixpkgs-unstable#nomad"
          ];
        } ''
          ${clone pr}

          cue export -e jobs.cicero \
            ${
              if environment == null then
                ""
              else
                "-t environment=${environment}"
            } \
            -t 'sha=${pr.commit.sha}' \
            > job.json
          nomad run job.json
        '');
      };
  };
}
