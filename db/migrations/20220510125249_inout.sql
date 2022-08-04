-- migrate:up

DROP TABLE run_output;

DROP VIEW api.current_action;

ALTER TABLE action
DROP inputs,
ADD io text NOT NULL;

CREATE VIEW api.current_action AS
SELECT DISTINCT ON (name) *
FROM public.action
ORDER BY name, created_at DESC;

CREATE TYPE run_status AS ENUM ('running', 'succeeded', 'failed', 'canceled');

ALTER TABLE run
ADD status run_status NOT NULL CHECK(status IN ('running', 'canceled') OR finished_at IS NOT NULL);
