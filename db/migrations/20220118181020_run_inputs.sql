-- migrate:up

CREATE TABLE run_inputs (
	run_id uuid REFERENCES run (nomad_job_id) ON DELETE CASCADE,
	input_name text NOT NULL,
	fact_id uuid NOT NULL REFERENCES fact (id)
);

-- migrate:down

DROP TABLE run_inputs;
