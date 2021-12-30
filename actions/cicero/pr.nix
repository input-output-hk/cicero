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

  outputs = { github-event }: {
    success.${namespace}.start = {
      inherit (github-event.pull_request.head.repo) clone_url;
      inherit (github-event.pull_request.head) sha;
      statuses_url = github-event.pull_request._links.statuses.href;
    };
  };
}
