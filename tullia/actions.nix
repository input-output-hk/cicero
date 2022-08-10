{
  "cicero/ci" = {
    task = "build";
    io = ''
      let github = {
        #input: "GitHub event"
        #repo: "input-output-hk/cicero"
      }

      #lib: ios: [
        {#lib.io.github_pr,   github},
        {#lib.io.github_push, github},
      ]
    '';
  };
}
