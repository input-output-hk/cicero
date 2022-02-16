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
			memory: 4096
			cpu:    300
		}

		env: {
			DATABASE_URL: #databaseUrl
			NOMAD_ADDR:   #nomadAddr
			VAULT_ADDR:   #vaultAddr
		}

		_transformers: [...]

		config: {
			// arbitrary revision from nixpkgs-unstable
			let nixpkgsRev = "19574af0af3ffaf7c9e359744ed32556f34536bd"

			packages: [
				#ciceroFlake,
				// for transformers
				"github:NixOS/nixpkgs/\(nixpkgsRev)#dash",
				"github:NixOS/nixpkgs/\(nixpkgsRev)#jq",
			]

			command: [
				"/bin/entrypoint",
				"--prometheus-addr", #lokiAddr,
				"--transform", for t in _transformers { t.destination },
				"--web-listen", ":8888",
			]
		}
	}
}

let commonTransformers = [{
	destination: "local/transformer.sh"
	perms: "544"
	data: """
		#! /bin/dash
		/bin/jq '
			.job[]?.datacenters |= . + ["dc1"] |
			.job[]?.group[]?.restart.attempts = 0 |
			.job[]?.group[]?.task[]?.env |= . + {
				NOMAD_ADDR:  env.NOMAD_ADDR,
				NOMAD_TOKEN: env.NOMAD_TOKEN,
			} |
			.job[]?.group[]?.task[]?.vault.policies |= . + ["cicero"]
		'
		"""
}]

if #env != "prod" {
	job: cicero: group: cicero: task: cicero: {
		_transformers: commonTransformers
		template: _transformers
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

			task: cicero: {
				vault: {
					policies: ["cicero"]
					change_mode: "restart"
				}

				_transformers: commonTransformers + [{
					destination: "local/transformer-prod.sh"
					perms: "544"
					data: """
						#! /bin/dash
						/bin/jq '
							.job[]?.datacenters |= . + ["eu-central-1", "us-east-2"] |
							.job[]?.group[]?.task[]?.env.CICERO_WEB_URL = "https://cicero.infra.aws.iohkdev.io"
						'
						"""
				}]

				template: _transformers + [
					{
						destination: "secrets/netrc"
						data: """
							machine github.com
							login git
							password {{with secret "kv/data/cicero/github"}}{{.Data.data.token}}{{end}}
							"""
					},
					{
						destination: "/root/.config/git/config"
						data: """
							[credential]
								helper = netrc -vkf /secrets/netrc
							"""
					}
				]

				env: NETRC: "/secrets/netrc"
			}
		}
	}
}
