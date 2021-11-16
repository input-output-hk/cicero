package jobs

import (
	"encoding/json"
)

job: webhooks: group: webhooks: {
	restart: {
		attempts: 5
		delay:    "10s"
		interval: "1m"
		mode:     "delay"
	}

	reschedule: {
		delay:          "10s"
		delay_function: "exponential"
		max_delay:      "1m"
		unlimited:      true
	}

	network: {
		mode: "host"
		port: http: static: "4567"
	}

	service: [{
		name:         "webhooks"
		address_mode: "auto"
		port:         "http"
		tags: [
			"webhooks",
			"ingress",
			"traefik.enable=true",
			"traefik.http.routers.webhooks.rule=Host(`webhooks.infra.aws.iohkdev.io`) && PathPrefix(`/`)",
			"traefik.http.routers.webhooks.entrypoints=https",
			"traefik.http.routers.webhooks.tls=true",
		]
		check: [{
			type:     "tcp"
			port:     "http"
			interval: "10s"
			timeout:  "2s"
		}]
	}]

	task: webhooks: {
		driver: "nix"

		resources: {
			memory: 512
			cpu:    300
		}

		env: {
			NOMAD_ADDR: #nomadAddr
			VAULT_ADDR: #vaultAddr
		}

		config: {
			packages: [
				#webhookFlake,
				"github:nixos/nixpkgs/nixpkgs-unstable#bash",
				"github:nixos/nixpkgs/nixpkgs-unstable#jq",
				"github:nixos/nixpkgs/nixpkgs-unstable#curl",
			]
			command: ["/bin/trigger", "--config", "/local/trigger.yaml"]
		}

		template: [{
			destination: "/local/trigger.yaml"
			data:        json.Marshal({
				settings: {
					host:           "0.0.0.0:4567"
					print_commands: true
					capture_output: false
					secret:         "TODO get from vault"
				}
				events: {
					common: """
						set -exuo pipefail
						function prop {
						    <<< '{payload}' jq -r "$1"
						}
						"""
					pull_request: """
						case $(prop .action) in
						    opened | reopened | synchronize ) ;;
						    * ) exit 0 ;;
						esac

						if [[ $(prop .pull_request.base.ref) != $(prop .repository.default_branch) ]]; then
						    >&2 echo 'Ignoring event: PR base is not the default branch. This could allow arbitrary code execution.'
						    exit
						fi

						echo "nameserver \(#nameserver)" > /etc/resolv.conf

						<<< '{payload}' jq -r '{
						    Source: "github:\\(.repository.full_name)/\\(.pull_request.base.sha)",
						    Inputs: {"github-event": .}
						}' | curl "\(#ciceroApiUrl)/workflow/instance/" --data-binary @-
						"""
				}
			})
		}]
	}
}
