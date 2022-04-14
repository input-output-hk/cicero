# Ugly wrapper script for `cue fmt` that adheres to the treefmt spec.
# https://github.com/numtide/treefmt/issues/140
{
  lib,
  writers,
  cue,
  gitMinimal,
}:
writers.writeBashBin "treefmt-cue" ''
  set -euo pipefail

  PATH="$PATH:"${lib.makeBinPath [
    gitMinimal
    cue
  ]}

  trap 'rm -rf "$tmp"' EXIT
  tmp="$(mktemp -d)"

  root="$(git rev-parse --show-toplevel)"

  for f in "$@"; do
    fdir="$tmp"/"$(dirname "''${f#"$root"/}")"
    mkdir -p "$fdir"
    cp -a "$f" "$fdir"/
  done
  cp -ar "$root"/.git "$tmp"/

  cd "$tmp"
  cue fmt "''${@#"$root"/}"

  for f in "''${@#"$root"/}"; do
    if [ -n "$(git status --porcelain --untracked-files=no -- "$f")" ]; then
      cp "$f" "$root"/"$f"
    fi
  done
''
