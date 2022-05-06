package jobs

// arbitrary revision from nixpkgs-unstable
let nixpkgsRev = "19574af0af3ffaf7c9e359744ed32556f34536bd"

let commonTransformers = [{
	destination: "local/transformer.sh"
	perms:       "544"
	data: """
		#! /bin/bash
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

job: cicero: group: {
	[string]: {
		restart: {
			attempts: 5
			delay:    "10s"
			interval: "1m"
			mode:     "delay"
		}

		update: {
			canary:       1
			auto_promote: true
			auto_revert:  true
		}

		reschedule: {
			delay:          "10s"
			delay_function: "exponential"
			max_delay:      "1m"
			unlimited:      true
		}

		task: [string]: {
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

			config: packages: [
				#ciceroFlake,
				// for transformers
				"github:NixOS/nixpkgs/\(nixpkgsRev)#bash",
				"github:NixOS/nixpkgs/\(nixpkgsRev)#jq",
			]
		}
	}

	cicero: {
		network: port: http: {}

		task: cicero: {
			_transformers: [...]

			config: command: [
				"/bin/entrypoint",
				"--victoriametrics-addr", #victoriaAddr,
				"--prometheus-addr", #lokiAddr,
				"--transform", for t in _transformers {t.destination},
				"--web-listen", ":${NOMAD_PORT_http}",
			]
		}
	}

	"cicero-nomad": {
		count: 3

		task: "cicero-nomad": {
			_transformers: [...]

			config: command: [
				"/bin/entrypoint", "nomad",
				"--transform", for t in _transformers {t.destination},
			]
		}
	}
}

if #env != "prod" {
	job: cicero: group: [string]: task: [string]: {
		_transformers: commonTransformers
		template:      _transformers
	}
}

if #env == "prod" {
	job: cicero: {
		namespace: "cicero"

		group: {
			cicero: service: [
				{
					name:         "cicero-internal"
					address_mode: "auto"
					port:         "http"
					tags: [
						"cicero",
						"ingress",
						"traefik.enable=true",
						"traefik.http.routers.cicero-internal.rule=Host(`cicero.infra.aws.iohkdev.io`) && HeadersRegexp(`Authorization`, `Basic`)",
						"traefik.http.routers.cicero-internal.middlewares=cicero-auth@consulcatalog",
						"traefik.http.middlewares.cicero-auth.basicauth.users=cicero:$2y$05$lcwzbToms.S83xjBFlHSvO.Lt3Y37b8SLd/9aYuqoSxBOxR9693.2",
						"traefik.http.middlewares.cicero-auth.basicauth.realm=Cicero",
						"traefik.http.routers.cicero-internal.entrypoints=https",
						"traefik.http.routers.cicero-internal.tls=true",
						"traefik.http.routers.cicero-internal.tls.certresolver=acme",
					]
					canary_tags: ["cicero"]
					check: [{
						type:     "tcp"
						port:     "http"
						interval: "10s"
						timeout:  "2s"
					}]
				},
				{
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
						"traefik.http.routers.cicero.tls.certresolver=acme",
					]
					canary_tags: ["cicero"]
					check: [{
						type:     "tcp"
						port:     "http"
						interval: "10s"
						timeout:  "2s"
					}]
				},
			]

			[string]: task: [string]: {
				vault: {
					policies: ["cicero"]
					change_mode: "restart"
				}

				_transformers: commonTransformers + [{
					destination: "local/transformer-prod.sh"
					perms:       "544"
					data:        """
						#! /bin/bash
						/bin/jq '
							.job[]?.datacenters |= . + ["eu-central-1", "us-east-2"] |
							.job[]?.group[]?.task[]? |= if .config?.nixos then . else (
								.env |= . + {
									CICERO_WEB_URL: "https://cicero.infra.aws.iohkdev.io",
									NIX_CONFIG: (
										"extra-substituters = http://spongix.service.consul:7745?compression=none\n" +
										"extra-trusted-public-keys =" +
											" infra-production-0:T7ZxFWDaNjyEiiYDe6uZn0eq+77gORGkdec+kYwaB1M=" +
											" hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" +
											"\n" +
										"post-build-hook = /local/post-build-hook\n" +
										.NIX_CONFIG
									),
								} |
								.config |=
									if has("packages")
									then .packages |=
										# only add bash if needed to avoid conflicts in profile
										if any(endswith("#bash"))
										then .
										else . + ["github:NixOS/nixpkgs/\(nixpkgsRev)#bash"]
										end
									else .
									end |
								.template |= . + [{
									destination: "local/post-build-hook",
									perms: "544",
									data: (
										"#! /bin/bash\\n" +
										"set -euf\\n" +
										"export IFS=\\\" \\\"\\n" +
										"if [[ -n \\\"$OUT_PATHS\\\" ]]; then\\n" +
										"\\techo \\\"Uploading to cache: $OUT_PATHS\\\"\\n" +
										"\\texec nix copy --to \\\"http://spongix.service.consul:7745?compression=none\\\" $OUT_PATHS\\n" +
										"fi"
									),
								}]
							) end
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

							machine api.github.com
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
					},
				]

				env: {
					NIX_CONFIG: "netrc-file = /secrets/netrc"

					// go-getter reads from the NETRC env var or $HOME/.netrc
					// https://github.com/hashicorp/go-getter/blob/4553965d9c4a8d99bd0d381c1180c08e07eff5fd/netrc.go#L24
					NETRC: "/secrets/netrc"
				}
			}
		}
	}
}
