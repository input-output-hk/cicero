{...}: {
  io = ''
    inputs: "github-event": match: "github-event": {
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

    let event = inputs."github-event".value."github-event"
    output: success: "\(event.repository.name)/ci": start: {
      clone_url:    event.pull_request.head.repo.clone_url
      sha:          event.pull_request.head.sha
      statuses_url: event.pull_request."_links".statuses.href
    }
  '';
}
