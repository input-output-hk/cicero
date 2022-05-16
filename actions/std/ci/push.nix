{...}: {
  io = ''
    inputs: "github-event": match: "github-event": {
      pusher: {}
      deleted: false
      ref: string
      repository: {
        name: string
        owner: login: string
        default_branch: string
        clone_url: string
      }
      head_commit: id: string
    }

    let event = inputs."github-event".value."github-event"
    output: success: "\(event.repository.name)/ci": start: {
      clone_url:      event.repository.clone_url
      default_branch: event.repository.default_branch
      ref:            event.ref
      sha:            event.head_commit.id
      statuses_url: "https://api.github.com/repos/\(event.repository.owner.login)/\(event.repository.name)/statuses/\(event.head_commit.id)"
    }
  '';
}
