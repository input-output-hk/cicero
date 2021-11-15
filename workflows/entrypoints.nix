{ id, run }:

let lib = import ../workflows-lib.nix;
in {
  actions = {
    github = { sha ? null, environment ? null, github ? null }: {
      when = {
        "sha given" = sha != null;
        "github hasn't run yet" = github == null;
      };

      job = lib.addNomadJobDefaults (run "bash" {
        memory = 1024;
        packages = [
          "github:nixos/nixpkgs/nixpkgs-unstable#cacert"
          "github:nixos/nixpkgs/nixpkgs-unstable#coreutils"
          "github:nixos/nixpkgs/nixpkgs-unstable#cue"
          "github:nixos/nixpkgs/nixpkgs-unstable#git"
          "github:nixos/nixpkgs/nixpkgs-unstable#gnutar"
          "github:nixos/nixpkgs/nixpkgs-unstable#nomad"
        ];
      } ''
        set -exuo pipefail

        export SSL_CERT_FILE="/current-profile/etc/ssl/certs/ca-bundle.crt"
        export HOME="$PWD/.home"

        mkdir "$HOME"

        git config --global advice.detachedHead false
        git clone --quiet https://github.com/input-output-hk/cicero src
        cd src
        git checkout ${sha}

        cue export -e jobs.webhooks \
          ${if environment == null then "" else "-t env=${environment}"} \
          -t 'sha=${sha}' \
          > job.json

        nomad run job.json
      '');
    };
  };
}
