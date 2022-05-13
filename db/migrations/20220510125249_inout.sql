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
