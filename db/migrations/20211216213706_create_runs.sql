-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE runs (
	nomad_job_id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
	action_id uuid NOT NULL,
	created_at timestamp NOT NULL DEFAULT NOW(),
	finished_at timestamp,
	FOREIGN KEY (action_id) REFERENCES actions (id)
);

CREATE TABLE run_outputs (
	run_id uuid PRIMARY KEY,
	success jsonb,
	failure jsonb,
	FOREIGN KEY (run_id) REFERENCES runs (nomad_job_id)
);

-- migrate:down

DROP TABLE runs, run_outputs;
