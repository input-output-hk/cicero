-- migrate:up

ALTER TABLE nomad_event
ADD uid BYTEA GENERATED ALWAYS AS (digest(topic || type || payload::text, 'md5')) STORED UNIQUE,
ADD handled BOOLEAN NOT NULL DEFAULT FALSE;

UPDATE nomad_event SET handled = TRUE;

-- migrate:down

ALTER TABLE nomad_event
DROP uid,
DROP handled;
