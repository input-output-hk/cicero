-- migrate:up

CREATE EXTENSION IF NOT EXISTS lo;

ALTER TABLE facts
ADD COLUMN "binary" lo CHECK (("binary" IS NULL) = (binary_hash IS NULL));

-- migrate:down

ALTER TABLE facts
DROP COLUMN "binary";
