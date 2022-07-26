{
  "cicero/ci" = {
    task = "build";
    io = ''
      _lib: github: {
        #repo: "input-output-hk/cicero"
        pull_request: {}
        push: {}
      }
    '';
  };
}
