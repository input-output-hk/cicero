package jobs

#env:            string | *"dev" | "prod"              @tag(env)
#sha:            string                                @tag(sha)
#ciceroFlake:    string | *#environment.ciceroFlake    @tag(ciceroFlake)
#nomadAddr:      string | *#environment.nomadAddr      @tag(nomadAddr)
#liftbridgeAddr: string | *#environment.liftbridgeAddr @tag(liftbridgeAddr)
#databaseUrl:    string | *#environment.databaseUrl    @tag(databaseUrl)
#vaultAddr:      string | *#environment.vaultAddr      @tag(vaultAddr)

#environment: #environments[#env]

#environments: {
	dev: {
		ciceroFlake:    "path:.#cicero-entrypoint"
		databaseUrl:    "postgres://postgres:@127.0.0.1:5432/cicero?sslmode=disable"
		liftbridgeAddr: "127.0.0.1:9292"
		nomadAddr:      "http://127.0.0.1:4646"
		vaultAddr:      "http://127.0.0.1:8300"
	}

	prod: {
		ciceroFlake:    "github:input-output-hk/cicero/\(#sha)#cicero-entrypoint"
		databaseUrl:    "postgres://cicero:@hydra.node.consul:5432/cicero?sslmode=disable"
		liftbridgeAddr: "liftbridge.service.consul:9292"
		nomadAddr:      "https://nomad.infra.aws.iohkdev.io"
		vaultAddr:      "https://vault.infra.aws.iohkdev.io"
	}
}

job: [string]: {
	id?:  string
	type: "service"
	datacenters: ["dc1", "eu-central-1", "us-east-2"]

	namespace: "cicero" | *"default"

	group: [string]: {
		task: [string]: {
			driver: "nix"
		}
	}
}

for jobName, jobValue in job {
	jobs: "\(jobName)": job: "\(jobName)": jobValue
}
