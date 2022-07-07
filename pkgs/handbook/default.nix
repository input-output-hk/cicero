{
  runCommand,
  mdbook,
  mdbook-mermaid,
}:
runCommand "cicero-handbook" {buildInputs = [mdbook mdbook-mermaid];} ''
  mdbook build --dest-dir "$out" ${../..}
''
