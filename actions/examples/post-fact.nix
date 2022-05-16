{
  name,
  std,
  lib,
  actionLib,
  ...
} @ args: {
  io = ''
    inputs: start: match: "${name}": exit: _ // TODO make `uint` work
  '';

  job = {start}:
    std.chain args [
      actionLib.simpleJob
      std.postFact
      (std.script "bash" ''
        > /local/cicero/post-fact/success/fact echo '{"actions/examples/post-fact": {"ok": true}}'
        > /local/cicero/post-fact/success/artifact echo artifact
        > /local/cicero/post-fact/failure/fact echo '{"actions/examples/post-fact": {"ok": false}}'
        exit ${lib.escapeShellArg start.value.${name}.exit}
      '')
    ];
}
