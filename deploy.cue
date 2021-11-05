package deploy

import (
	"encoding/yaml"
)

#ciceroFlake: string | *"path:.#cicero-entrypoint" @tag(ciceroFlake)
#nomadAddr:   string | *"http://127.0.0.1:4646"    @tag(nomadAddr)

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
			liftbridge: {
				network: {
					mode: "host"
					// clients connections
					port: liftbridge: static: "9292"
					// clients connections
					port: nats: static: "4222"
					// HTTP management port for information reporting and monitoring
					port: http: static: "8222"
					// routing port for clustering
					port: routing: static: "6222"
				}

				task: liftbridge: {
					driver: "nix"

					resources: {
						memory: 128
						cpu:    200
					}

					config: [{
						packages: ["github:input-output-hk/cicero#liftbridge"]
						command: ["/bin/liftbridge", "--config", "/local/config.yaml"]
					}]

					template: [{
						destination: "/local/config.yaml"
						data:        yaml.Marshal({
							listen: "0.0.0.0:9292"
							host:   "127.0.0.1"
							port:   "9292"
							data: dir: "/local/server"
							activity: stream: enabled: true
							logging: {
								level:    "debug"
								raft:     true
								nats:     true
								recovery: true
							}
							nats: {
								embedded: true
								servers: []
							}
							streams: {
								retention: max: {
									age:      "24h"
									messages: 1000
								}
								compact: enabled: true
							}
							clustering: {
								server: id: "voter"
								raft: bootstrap: seed: true
								replica: max: lag: time: "20s"
							}
						})
					}]
				}
			}

			postgres: {
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
					port: psql: static: "5432"
				}

				task: postgres: {
					driver: "nix"

					resources: {
						cpu:    1000
						memory: 1024
					}

					kill_timeout: "60s"

					config: {
						nixos: "path:.#nixosConfigurations.postgres"
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
							"--liftbridge-addr", "liftbridge.service.consul:9292",
							"--listen", ":8888",
						]
					}]
				}
			}
		}
	}
}
