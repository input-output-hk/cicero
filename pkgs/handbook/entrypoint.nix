{
  writeShellScriptBin,
  darkhttpd,
  handbook-entrypoint,
}:
writeShellScriptBin "serve-cicero-handbook" ''
  exec darkhttpd ${handbook-entrypoint} --port "''${NOMAD_PORT_http:-8080}"
''
