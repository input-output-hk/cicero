package jobs

import (
	"encoding/yaml"
)

job: {
	cicero: {
		type: "service"

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

			// cicero: {
			//  restart: {
			//   attempts: 5
			//   delay:    "10s"
			//   interval: "1m"
			//   mode:     "delay"
			//  }

			//  reschedule: {
			//   delay:          "10s"
			//   delay_function: "exponential"
			//   max_delay:      "1m"
			//   unlimited:      true
			//  }

			//  network: {
			//   mode: "host"
			//   port: http: static: "8080"
			//  }

			//  task: cicero: {
			//   driver: "nix"

			//   resources: {
			//    memory: 1024
			//    cpu:    300
			//   }

			//   env: {
			//    DATABASE_URL:  "postgres://postgres:@127.0.0.1:5432/cicero?sslmode=disable"
			//    NIX_CONFIG:    "experimental-features = nix-command flakes"
			//    SSL_CERT_FILE: "/current-profile/etc/ssl/certs/ca-bundle.crt"
			//    NOMAD_ADDR:    "https://nomad.infra.aws.iohkdev.io"
			//   }

			//   config: [{
			//    packages: [
			//     "path:.#cicero",
			//     "path:.#cicero-evaluator-nix",
			//     "path:.#wfs",
			//     "github:nixos/nixpkgs/nixpkgs-unstable#nixUnstable",
			//     "github:nixos/nixpkgs/nixpkgs-unstable#bash",
			//     "github:nixos/nixpkgs/nixpkgs-unstable#coreutils",
			//     "github:nixos/nixpkgs/nixpkgs-unstable#shadow",
			//     "github:nixos/nixpkgs/nixpkgs-unstable#git",
			//     "github:nixos/nixpkgs/nixpkgs-unstable#cacert",
			//     "github:nixos/nixpkgs/nixpkgs-unstable#dbmate",
			//    ]

			//    command: ["/bin/bash", "/local/entrypoint.sh"]
			//   }]

			//   template: [{
			//    destination: "/local/entrypoint.sh"
			//    data: """
			//    set -exuo pipefail

			//    env

			//    mkdir -p /etc
			//    echo 'nixbld:x:30000:nixbld1' > /etc/group
			//    echo 'nixbld1:x:30001:30000:Nix build user 1:/current-profile/var/empty:/bin/nologin' > /etc/passwd
			//    nix-store --load-db < /registration

			//    git clone --quiet --depth 1 https://github.com/input-output-hk/cicero
			//    cd cicero
			//    dbmate up

			//    exec /bin/cicero all --liftbridge-addr 127.0.0.1:9292
			//    """
			//   }]
			//  }
			// }
		}
	}
}
