{ id, workflow }:
let
  length = arg: builtins.length (builtins.attrNames arg);
  any = arg: (length arg) > 0;
in workflow {
  name = "github";
  version = 0;
  tasks = {
    clone = { pr ? { } }: {
      when = { "we got a PR" = any pr; };
      run = ''
        mkdir -p "$(dirname ${pr.repository.full_name}).${id}"
        git clone ${pr.repository.clone_url} ${pr.repository.full_name}.${id}
      '';
    };

    checkout = { pr ? { }, clone ? false }: {
      when = {
        "we got a PR" = any pr;
        "repository was cloned" = clone;
      };

      success = {
        checkout = true;
        sha = (pr.commit or { }).sha or "<unknown>";
      };

      run = ''
        cd ${pr.repository.full_name}.${id}
        git remote update
        git checkout ${pr.commit.sha}
      '';
    };

    test = { pr ? { }, test ? false, checkout ? false, sha ? null }: {
      when = {
        "we got a PR" = any pr;
        "is checked out" = checkout;
        "test didn't run yet" = !test;
        "received a new commit" = sha != null;
      };

      run = ''
        cd ${pr.repository.full_name}.${id}
        git checkout ${pr.commit.sha}
      '';
    };
  };
}
