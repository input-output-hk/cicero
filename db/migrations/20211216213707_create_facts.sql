-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS lo;

CREATE TABLE fact (
	id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
	run_id uuid,
	value jsonb NOT NULL,
	binary_hash text,
	"binary" lo CHECK (("binary" IS NULL) = (binary_hash IS NULL)),
	created_at timestamp NOT NULL DEFAULT NOW(),
	FOREIGN KEY (run_id) REFERENCES run (nomad_job_id)
);

CREATE TRIGGER "binary" BEFORE UPDATE OR DELETE ON fact
FOR EACH ROW EXECUTE FUNCTION lo_manage("binary");

-- migrate:down

DROP TABLE fact;
