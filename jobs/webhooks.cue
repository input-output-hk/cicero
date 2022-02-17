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
		port: http: {}
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
				"github:nixos/nixpkgs/nixpkgs-unstable#dig",
				"github:nixos/nixpkgs/nixpkgs-unstable#coreutils",
			]
			command: ["/bin/trigger", "--config", "secrets/trigger.yaml"]
		}

		if #env == "prod" {
			vault: {
				policies: ["cicero"]
			}
		}

		template: [{
			destination: "secrets/trigger.yaml"
			data: #"""
				{
					"settings": {
						"host":           "0.0.0.0:{{env "NOMAD_PORT_http"}}",
						"print_commands": true,
						"capture_output": false,
						"secret":         "{{with secret "kv/data/cicero/github"}}{{.Data.data.webhooks}}{{end}}"
					},
					"events": \#(json.Marshal(_data_events))
				}
				"""#
			_data_events: all: #"""
				set -exuo pipefail

				echo "nameserver \#(#nameserver)" >> /etc/resolv.conf

				ciceroPort=$(dig +short cicero.service.consul SRV | cut -d ' ' -f 3)
				ciceroApiUrl="http://cicero.service.consul:$ciceroPort/api"

				<<< '{payload}' \
				jq -r '{"github-event": .}' \
				| curl "$ciceroApiUrl/fact" --data-binary @-
				"""#
		}]
	}
}
