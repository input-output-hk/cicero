-- migrate:up

ALTER TABLE liftbridge_messages
ALTER COLUMN value DROP NOT NULL;

-- migrate:down

ALTER TABLE liftbridge_messages
ALTER COLUMN value SET NOT NULL;
