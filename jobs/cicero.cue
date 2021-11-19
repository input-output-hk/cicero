package jobs

job: cicero: group: cicero: {
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
		port: http: static: "8888"
	}

	task: cicero: {
		driver: "nix"

		resources: {
			memory: 1024
			cpu:    300
		}

		env: {
			DATABASE_URL: #databaseUrl
			NOMAD_ADDR:   #nomadAddr
			VAULT_ADDR:   #vaultAddr
		}

		config: [{
			packages: [#ciceroFlake]
			command: [
				"/bin/entrypoint",
				"--liftbridge-addr", #liftbridgeAddr,
				"--prometheus-addr", #lokiAddr,
				"--env", "NOMAD_ADDR", "NOMAD_TOKEN",
				"--web-listen", ":8888",
			]
		}]
	}
}

if #env == "prod" {
	job: cicero: {
		namespace: "cicero"
		group: cicero: {
			service: [{
				name:         "cicero"
				address_mode: "auto"
				port:         "http"
				tags: [
					"cicero",
					"ingress",
					"traefik.enable=true",
					"traefik.http.routers.cicero.rule=Host(`cicero.infra.aws.iohkdev.io`)",
					"traefik.http.routers.cicero.middlewares=oauth-auth-redirect@file",
					"traefik.http.routers.cicero.entrypoints=https",
					"traefik.http.routers.cicero.tls=true",
				]
				check: [{
					type:     "tcp"
					port:     "http"
					interval: "10s"
					timeout:  "2s"
				}]
			}]

			task: cicero: vault: {
				policies: ["cicero"]
				change_mode: "restart"
			}
		}
	}
}
