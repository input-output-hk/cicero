-- migrate:up

CREATE INDEX IF NOT EXISTS index_nomad_event_job_id
  ON nomad_event ((payload #>> '{Allocation,JobID}'::text[]));

CREATE INDEX IF NOT EXISTS index_nomad_event_topic
  ON nomad_event (topic);

-- migrate:down

DROP INDEX IF EXISTS index_nomad_event_job_id;

DROP INDEX IF EXISTS index_nomad_event_topic;
