-- migrate:up

ALTER TABLE invocation
ADD finished_at timestamp;
