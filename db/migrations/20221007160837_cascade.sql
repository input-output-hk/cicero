-- migrate:up

ALTER TABLE fact
DROP CONSTRAINT fact_run_id_fkey,
ADD FOREIGN KEY (run_id) REFERENCES run (nomad_job_id) ON DELETE CASCADE;

-- migrate:down

ALTER TABLE fact
DROP CONSTRAINT fact_run_id_fkey,
ADD FOREIGN KEY (run_id) REFERENCES run (nomad_job_id);
