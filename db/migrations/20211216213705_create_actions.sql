-- migrate:up

DROP TABLE workflow_instances;
DROP TABLE action_instances;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE actions (
	id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
	name text NOT NULL CHECK (name <> ''),
	meta jsonb,
	source text NOT NULL CHECK (source <> ''),
	inputs jsonb NOT NULL,
	created_at timestamp NOT NULL DEFAULT NOW()
);
