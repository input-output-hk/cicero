package jobs

job: "ciceroHandbook": {
	namespace: "cicero"
	datacenters: ["dc1", "eu-central-1", "us-east-2"]
	group: handbook: {
		network: port: http: {}
		service: [
			{
				name: "cicero-handbook"
				port: "http"
				tags: [
					"ingress",
					"traefik.enable=true",
					"traefik.http.routers.cicero-handbook.rule=Host(`cicero-handbook.infra.aws.iohkdev.io`) && PathPrefix(`/`)",
					"traefik.http.routers.cicero-handbook.entrypoints=https",
					"traefik.http.routers.cicero-handbook.middlewares=oauth-auth-redirect@file",
					"traefik.http.routers.cicero-handbook.tls=true",
					"traefik.http.routers.cicero-handbook.tls.certresolver=acme",
				]
				check: [
					{
						type:     "tcp"
						port:     "http"
						interval: "10s"
						timeout:  "2s"
					},
				]
			},
		]

		task: handbook: {
			driver: "nix"
			config: {
				packages: [#handbookFlake]
				command: ["/bin/serve-cicero-handbook"]
			}
		}

	}
}
