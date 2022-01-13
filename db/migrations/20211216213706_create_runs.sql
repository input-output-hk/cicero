-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE run (
	nomad_job_id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
	action_id uuid NOT NULL,
	created_at timestamp NOT NULL DEFAULT NOW(),
	finished_at timestamp,
	FOREIGN KEY (action_id) REFERENCES action (id)
);

CREATE TABLE run_output (
	run_id uuid PRIMARY KEY,
	success jsonb,
	failure jsonb,
	FOREIGN KEY (run_id) REFERENCES run (nomad_job_id)
);

-- migrate:down

DROP TABLE run, run_output;
