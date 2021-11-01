-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

create table step_instances (
  id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
  workflow_instance_id integer NOT NULL,
  name text NOT NULL,
  certs jsonb NOT NULL,
  created_at timestamp NOT NULL DEFAULT NOW(),
  finished_at timestamp
);

-- migrate:down

drop table step_instances;
