{
  name,
  std,
  lib,
  actionLib,
  ...
} @ args: {
  inputs = let
    common = ''
      ok: true

      // push to default branch
      ref?: "refs/heads/\(default_branch)"
      default_branch?: string
    '';
  in {
    lintAndBuild = ''
      "cicero/ci/lintAndBuild": {
        revision: string
        ${common}
      }
    '';

    schemathesis = ''
      "cicero/ci/schemathesis": {
        revision: _inputs.lintAndBuild.value."cicero/ci/lintAndBuild".revision
        ${common}
      }
    '';
  };

  output = {lintAndBuild, ...}: let
    cfg = lintAndBuild.value."cicero/ci/lintAndBuild";
  in {
    success.${name} =
      {
        ok = true;
        inherit (cfg) revision;
      }
      // lib.optionalAttrs (cfg ? ref) {
        inherit (cfg) ref default_branch;
      };
  };
}
