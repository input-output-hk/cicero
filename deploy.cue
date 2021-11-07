package deploy

#environment: string | *"local" @tag(env)
#sha:         string            @tag(sha)

#defaultEnvironments: {
	local: {
		ciceroFlake:    "path:.#cicero-entrypoint"
		nomadAddr:      "http://127.0.0.1:4646"
		liftbridgeAddr: "127.0.0.1:9292"
	}

	development: {
		ciceroFlake:    "github:input-output-hk/cicero/\(#sha)#cicero-entrypoint"
		nomadAddr:      local.nomadAddr
		liftbridgeAddr: local.liftbridgeAddr
	}

	production: {
		ciceroFlake:    "github:input-output-hk/cicero/\(#sha)#cicero-entrypoint"
		nomadAddr:      "https://nomad.infra.aws.iohkdev.io"
		liftbridgeAddr: "liftbridge.service.consul:9292"
	}
}

#defaultEnvironment: #defaultEnvironments[#environment]

#ciceroFlake:    string | *#defaultEnvironment.ciceroFlake    @tag(ciceroFlake)
#nomadAddr:      string | *#defaultEnvironment.nomadAddr      @tag(nomadAddr)
#liftbridgeAddr: string | *#defaultEnvironment.liftbridgeAddr @tag(liftbridgeAddr)

for jobName, jobValue in job {
	jobs: "\(jobName)": job: "\(jobName)": jobValue
}

job: [string]: {
	id?:  string
	type: "batch" | *"service"
	datacenters: ["dc1", "eu-central-1", "us-east-2"]
	namespace: "default"
	group: [string]: {
		task: [string]: {
			driver: "nix"
		}
	}
}

job: {
	dev: {
		group: {
			dev: {
				restart: {
					attempts: 5
					delay:    "1s"
					interval: "10s"
					mode:     "delay"
				}

				reschedule: {
					delay:          "10s"
					delay_function: "constant"
					max_delay:      "30s"
					unlimited:      true
				}

				network: {
					mode: "host"
					port: psql: static:       "5432"
					port: liftbridge: static: "9292"
					port: nats: static:       "4222"
					port: http: static:       "8222"
					port: routing: static:    "6222"
					port: loki: static:       "3100"
				}

				task: dev: {
					driver: "nix"

					resources: {
						cpu:    1000
						memory: 1024
					}

					kill_timeout: "60s"

					config: {
						nixos: "path:.#nixosConfigurations.dev"
					}
				}
			}
		}
	}

	cicero: {
		group: {
			cicero: {
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
						DATABASE_URL: "postgres://postgres:@127.0.0.1:5432/cicero?sslmode=disable"
						NOMAD_ADDR:   #nomadAddr
					}

					config: [{
						packages: [#ciceroFlake]
						command: [
							"/bin/entrypoint",
							"--liftbridge-addr", #liftbridgeAddr,
							"--listen", ":8888",
						]
					}]
				}
			}
		}
	}
}
