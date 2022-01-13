-- migrate:up

create table nomad_event (
	topic text NOT NULL,
	type text NOT NULL,
	key text NOT NULL,
	filter_keys jsonb NOT NULL,
	"index" integer check ("index" >= 0),
	payload jsonb NOT NULL
);

-- migrate:down

drop table nomad_event;
