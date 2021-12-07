{ self, ... }@args:

let
  inherit (self.lib) std;

  wfLib = import ../../workflows-lib.nix self;

  common = [ wfLib.jobDefaults std.singleTask ];
in std.callWorkflow args {
  actions = {
    bash = { }: {
      job = common ++ [
        (std.script "bash" ''
          echo 'Hello Bash'
        '')
      ];
    };

    python = { }: {
      job = common ++ [
        (std.script "python" ''
          print("Hello Python")
        '')
      ];
    };

    js = { }: {
      job = common ++ [
        (std.script "js" ''
          console.log('Hello JS')
        '')
      ];
    };

    perl = { }: {
      job = common ++ [
        (std.script "perl" ''
          print STDOUT "Hello Perl\n"
        '')
      ];
    };
  };
}
