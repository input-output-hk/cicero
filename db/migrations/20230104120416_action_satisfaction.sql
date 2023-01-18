-- migrate:up

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE action_satisfaction (
	action_id uuid NOT NULL,
	input_name text NOT NULL,
	fact_id uuid NOT NULL,
	PRIMARY KEY (action_id, input_name),
	FOREIGN KEY (action_id) REFERENCES action (id) ON DELETE CASCADE,
	FOREIGN KEY (fact_id) REFERENCES fact (id) ON DELETE CASCADE
);

-- migrate:down

DROP TABLE action_satisfaction;
