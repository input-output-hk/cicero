-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

create table action_instances (
  id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
  workflow_instance_id integer NOT NULL,
  name text NOT NULL,
  facts jsonb NOT NULL,
  created_at timestamp NOT NULL DEFAULT NOW(),
  updated_at timestamp,
  finished_at timestamp
);

-- migrate:down

drop table action_instances;
