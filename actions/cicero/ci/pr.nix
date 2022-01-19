{ name, std, ... } @ args:

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

  output = { github-event }:
    let inherit (github-event.value.github-event) pull_request; in
    {
      success."cicero/ci".start = {
        inherit (pull_request.head.repo) clone_url;
        inherit (pull_request.head) sha;
        statuses_url = pull_request._links.statuses.href;
      };
    };
}
