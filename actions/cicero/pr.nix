{ name, std, ... } @ args:

std.behavior.onInputChange "github-event" name args

{
  inputs.github-event = ''
    "github-event": {
      repository: {
        name: "cicero"
        owner: login: "input-output-hk"
        full_name?: "\(owner.login)/\(name)"
      }
      action: "opened" | "reopened" | "synchronize"
      pull_request: {
        "_links": statuses: href: string
        head: {
          sha: string
          repo: clone_url: string
        }
      }
    }
  '';

  outputs = { github-event }: let
    event = github-event.value.github-event;
  in {
    success."cicero/ci".start = {
      inherit (event.pull_request.head.repo) clone_url;
      inherit (event.pull_request.head) sha;
      statuses_url = event.pull_request._links.statuses.href;
    };
  };
}
