{ std, ... } @ args:

let namespace = "cicero"; in

std.behavior.onInputChange "github-event" namespace args

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
    success.${namespace}.start = {
      inherit (event.pull_request.head.repo) clone_url;
      inherit (event.pull_request.head) sha;
      statuses_url = event.pull_request._links.statuses.href;
    };
  };
}
