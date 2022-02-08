{ ... }:

{
  inputs.github-event = ''
    "github-event": {
      repository: {
        name: string
        owner: login: string
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
    let inherit (github-event.value.github-event) pull_request repository; in
    {
      success."${repository.name}/ci".start = {
        inherit (pull_request.head.repo) clone_url;
        inherit (pull_request.head) sha;
        statuses_url = pull_request._links.statuses.href;
      };
    };
}
