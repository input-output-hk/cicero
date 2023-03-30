-- migrate:up

ALTER TABLE nomad_event
ALTER COLUMN filter_keys SET DEFAULT 'null';

-- migrate:down

ALTER TABLE nomad_event
ALTER COLUMN filter_keys DROP DEFAULT;
