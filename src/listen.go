package cicero

import (
	"encoding/json"
	"log"
	"os"
	"net/http"
)

type ListenCmd struct {
	Only   string `arg:"--only" default:""`
	Addr   string `arg:"--listen" default:":8080"`
	logger *log.Logger
}

func (cmd *ListenCmd) init() {
	if cmd.logger == nil {
		cmd.logger = log.New(os.Stderr, "listen: ", log.LstdFlags)
	}
}

func (cmd *ListenCmd) Run() error {
	cmd.init()

	handleGithub := func(w http.ResponseWriter, req *http.Request) {
		switch req.Header.Get("X-GitHub-Event") {
		// default:
		// 	cmd.logger.Printf("unhandled or unknown event: %s\n", event)
		// case "pull_request":
		default:
			var body map[string]interface{}
			if err := json.NewDecoder(req.Body).Decode(&body); err != nil {
				cmd.logger.Print("failed to parse request", err)
			}
			cmd.logger.Printf("PARSED REQUEST", body)

			// publish(
			// 	cmd.logger,
			// 	fmt.Sprintf("workflow.%s.%d.invoke", workflow.Name, workflow.ID),
			// 	"workflow.*.*.invoke",
			// 	workflow.Certs,
			// )
		}
	}

	switch cmd.Only {
	case "github":
		http.HandleFunc("/", handleGithub)
	case "":
		http.HandleFunc("/github", handleGithub)
	default:
		cmd.logger.Fatalf("don't know how to listen to %s\n", cmd.Only)
	}

	if len(cmd.Only) > 0{
		cmd.logger.Printf("listening only for %s\n", cmd.Only)
	} else {
		cmd.logger.Print("listening for everything")
	}

	return http.ListenAndServe(cmd.Addr, nil)
}
