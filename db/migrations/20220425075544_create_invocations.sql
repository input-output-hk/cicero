-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE invocation (
	id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
	action_id uuid NOT NULL REFERENCES action (id) ON DELETE CASCADE,
	created_at timestamp NOT NULL DEFAULT STATEMENT_TIMESTAMP(),

	-- these are nullable because they are only interesting on error
	eval_stdout text,
	eval_stderr text,

	-- Just a help column for this migration.
	-- This is NOT part of the actual schema!
	run_id uuid NOT NULL REFERENCES run (nomad_job_id)
);

-- Insert an invocation for every run.

INSERT INTO invocation (action_id, created_at, eval_stdout, eval_stderr, run_id)
SELECT action_id, created_at, '', '', nomad_job_id
FROM run;

-- Make every run point to its invocation.

ALTER TABLE run
ADD invocation_id uuid REFERENCES invocation (id) ON DELETE CASCADE;

UPDATE run
SET invocation_id = (
	SELECT id
	FROM invocation
	WHERE invocation.run_id = run.nomad_job_id
);

-- We no longer need the help column.
ALTER TABLE invocation
DROP run_id;

ALTER TABLE run
DROP action_id;

-- Change run_inputs to invocation_inputs.

ALTER TABLE run_inputs
RENAME TO invocation_inputs;

ALTER TABLE invocation_inputs
ADD invocation_id uuid REFERENCES invocation (id) ON DELETE CASCADE;

UPDATE invocation_inputs
SET invocation_id = (
	SELECT invocation_id
	FROM run
	WHERE run.nomad_job_id = run_id
);

ALTER TABLE invocation_inputs
ALTER invocation_id SET NOT NULL;

ALTER TABLE invocation_inputs
DROP run_id;
