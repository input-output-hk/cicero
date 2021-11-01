package deploy

import (
	jobDefs "github.com/input-output-hk/cicero/jobs:jobs"
)

job: jobDefs.job

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
