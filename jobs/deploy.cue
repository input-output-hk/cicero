package jobs

#env:           string | *"dev" | "prod"             @tag(env)
#sha:           string                               @tag(sha)
#ciceroFlake:   string | *#environment.ciceroFlake   @tag(ciceroFlake)
#webhookFlake:  string | *#environment.webhookFlake  @tag(webhookFlake)
#handbookFlake: string | *#environment.handbookFlake @tag(handbookFlake)
#nomadAddr:     string | *#environment.nomadAddr     @tag(nomadAddr)
#databaseUrl:   string | *#environment.databaseUrl   @tag(databaseUrl)
#vaultAddr:     string | *#environment.vaultAddr     @tag(vaultAddr)
#nameserver:    string | *#environment.nameserver    @tag(nameserver)
#lokiAddr:      string | *#environment.lokiAddr      @tag(lokiAddr)
#namespace:     string | *#environment.namespace     @tag(namespace)

#environment: #environments[#env]

#environments: {
	dev: {
		namespace:     "default"
		ciceroFlake:   "path:.#cicero-entrypoint"
		webhookFlake:  "path:.#webhook-trigger"
		handbookFlake: "path:.#handbook-entrypoint"
		databaseUrl:   "postgres://postgres:@127.0.0.1:5432/cicero?sslmode=disable"
		nomadAddr:     "http://127.0.0.1:4646"
		vaultAddr:     "http://127.0.0.1:8300"
		nameserver:    "1.1.1.1"
		lokiAddr:      "http://127.0.0.1:3100"
	}

	prod: {
		namespace:     "cicero"
		ciceroFlake:   "github:input-output-hk/cicero/\(#sha)#cicero-entrypoint"
		webhookFlake:  "github:input-output-hk/cicero/\(#sha)#webhook-trigger"
		handbookFlake: "github:input-output-hk/cicero/\(#sha)#handbook-entrypoint"
		databaseUrl:   "postgres://cicero:@hydra.node.consul:5432/cicero?sslmode=disable"
		nomadAddr:     "https://nomad.infra.aws.iohkdev.io"
		vaultAddr:     "https://vault.infra.aws.iohkdev.io"
		nameserver:    "172.17.0.1"
		lokiAddr:      "http://monitoring.node.consul:3100"
	}
}

job: [string]: {
	id?:  string
	type: *"service" | "batch"
	datacenters: ["dc1", "eu-central-1", "us-east-2"]

	namespace: #namespace

	group: [string]: {
		task: [string]: {
			driver: "nix"
		}
	}
}

for jobName, jobValue in job {
	jobs: "\(jobName)": job: "\(jobName)": jobValue
}
