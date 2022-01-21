-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE action (
	id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
	name text NOT NULL CHECK (name <> ''),
	meta jsonb,
	source text NOT NULL CHECK (source <> ''),
	inputs jsonb NOT NULL,
	created_at timestamp NOT NULL DEFAULT NOW()
);

-- migrate:down

DROP TABLE action;
