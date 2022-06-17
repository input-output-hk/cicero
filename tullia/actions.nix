{
  "cicero/ci" = {
    task = "schemathesis";
    io = ''
      _lib: github: {
        #repo: "input-output-hk/cicero"
        pull_request: {}
        push: {}
      }
    '';
  };
}
