package jobs

job: dev: group: dev: {
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
