-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE runs (
	nomad_job_id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
	action_id uuid NOT NULL,
	success jsonb NOT NULL,
	failure jsonb,
	created_at timestamp NOT NULL DEFAULT NOW(),
	finished_at timestamp,
	FOREIGN KEY (action_id) REFERENCES actions (id)
);

-- migrate:down

DROP TABLE runs;
