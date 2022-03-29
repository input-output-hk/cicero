{
  writeShellScriptBin,
  darkhttpd,
  handbook,
}:
writeShellScriptBin "serve-cicero-handbook" ''
  exec ${darkhttpd}/bin/darkhttpd ${handbook} --port "''${NOMAD_PORT_http:-8080}"
''
