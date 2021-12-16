-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE facts (
	id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
	run_id uuid,
	value jsonb NOT NULL,
	binary_hash text,
	created_at timestamp NOT NULL DEFAULT NOW(),
	FOREIGN KEY (run_id) REFERENCES runs (nomad_job_id)
);

-- migrate:down

DROP TABLE facts;
