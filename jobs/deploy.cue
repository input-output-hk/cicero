package jobs

#env:            string | *"dev" | "prod"              @tag(env)
#sha:            string                                @tag(sha)
#ciceroFlake:    string | *#environment.ciceroFlake    @tag(ciceroFlake)
#webhookFlake:   string | *#environment.webhookFlake   @tag(webhookFlake)
#nomadAddr:      string | *#environment.nomadAddr      @tag(nomadAddr)
#liftbridgeAddr: string | *#environment.liftbridgeAddr @tag(liftbridgeAddr)
#databaseUrl:    string | *#environment.databaseUrl    @tag(databaseUrl)
#vaultAddr:      string | *#environment.vaultAddr      @tag(vaultAddr)
#ciceroApiUrl:   string | *#environment.ciceroApiUrl   @tag(ciceroApiUrl)
#nameserver:     string | *#environment.nameserver     @tag(nameserver)
#lokiAddr:       string | *#environment.lokiAddr       @tag(lokiAddr)

#environment: #environments[#env]

#environments: {
	dev: {
		ciceroFlake:    "path:.#cicero-entrypoint"
		webhookFlake:   "path:.#webhook-trigger"
		databaseUrl:    "postgres://postgres:@127.0.0.1:5432/cicero?sslmode=disable"
		liftbridgeAddr: "127.0.0.1:9292"
		nomadAddr:      "http://127.0.0.1:4646"
		vaultAddr:      "http://127.0.0.1:8300"
		ciceroApiUrl:   "http://127.0.0.1:8080/api"
		nameserver:     "1.1.1.1"
		lokiAddr:       "http://127.0.0.1:3100"
	}

	prod: {
		ciceroFlake:    "github:input-output-hk/cicero/\(#sha)#cicero-entrypoint"
		webhookFlake:   "github:input-output-hk/cicero/\(#sha)#webhook-trigger"
		databaseUrl:    "postgres://cicero:@hydra.node.consul:5432/cicero?sslmode=disable"
		liftbridgeAddr: "liftbridge.service.consul:9292"
		nomadAddr:      "https://nomad.infra.aws.iohkdev.io"
		vaultAddr:      "https://vault.infra.aws.iohkdev.io"
		ciceroApiUrl:   "http://cicero.service.consul:8888/api"
		nameserver:     "172.17.0.1"
		lokiAddr:       "http://monitoring.node.consul:3100"
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
