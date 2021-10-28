-- migrate:up

create table step_instances (
	id blob primary key unique not null,
	workflow_instance_id integer not null,
	name text not null,
	certs jsonb not null,
	created_at text not null,
	finished_at text
);

-- migrate:down

drop table step_instances;
