{
  name,
  std,
  lib,
  actionLib,
  ...
} @ args: {
  io = ''
    inputs: {
      lintAndBuild: match: "cicero/ci/lintAndBuild": revision: string

      schemathesis: match: "cicero/ci/schemathesis": revision: inputs.lintAndBuild.value."cicero/ci/lintAndBuild".revision

      [string]: match: [string]: {
        ok: true

        // push to default branch
        ref?: "refs/heads/\(default_branch)"
        default_branch?: string
      }
    }

    let cfg = inputs.lintAndBuild.value."cicero/ci/lintAndBuild"
    output: success: "${name}": {
      ok:       true
      revision: cfg.revision

      if cfg.ref != _|_ {
        ref:            cfg.ref
        default_branch: cfg.default_branch
      }
    }
  '';
}
