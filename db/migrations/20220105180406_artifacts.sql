-- migrate:up

CREATE EXTENSION IF NOT EXISTS lo;

ALTER TABLE facts
ADD COLUMN "binary" lo CHECK (("binary" IS NULL) = (binary_hash IS NULL));

CREATE TRIGGER "binary" BEFORE UPDATE OR DELETE ON facts
FOR EACH ROW EXECUTE FUNCTION lo_manage("binary");

-- migrate:down

ALTER TABLE facts
DROP COLUMN "binary";

DROP TRIGGER "binary" ON facts;
