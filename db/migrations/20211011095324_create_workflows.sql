-- migrate:up

create table workflows (
  id bigint primary key unique not null,
  name text not null,
  certs jsonb not null
);

-- migrate:down

drop table workflows;
