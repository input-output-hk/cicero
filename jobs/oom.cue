package jobs

// A simple job that runs out of memory.
job: oom: {
	type: "batch"

	group: oom: {
		restart: {
			attempts: 0
		}

		reschedule: {
			attempts: 0
		}

		task: oom: {
			driver: "nix"

			resources: {
				cpu:    1000
				memory: 32
			}

			kill_timeout: "60s"

			config: {
				packages: [
					"github:nixos/nixpkgs/nixpkgs-unstable#bash",
				]
				command: ["/bin/bash", "-c", "for b in {0..99999999}; do a=$b$a; done"]
			}
		}
	}
}
