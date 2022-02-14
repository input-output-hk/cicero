-- migrate:up

ALTER TABLE action
ADD active BOOLEAN DEFAULT true;

-- migrate:down

ALTER TABLE action
DROP active;
