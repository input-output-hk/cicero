-- migrate:up

create table workflows (
  id integer primary key unique not null,
  name text not null,
  certs jsonb not null,
  created_at text not null,
  updated_at text not null
);

-- migrate:down

drop table workflows;
