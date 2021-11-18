-- migrate:up

create table workflow_instances (
  id bigint PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  name text NOT NULL,
  source text NOT NULL CHECK (source <> ''),
  facts jsonb NOT NULL,
  created_at timestamp NOT NULL DEFAULT NOW(),
  updated_at timestamp
);

-- migrate:down

drop table workflow_instances;
