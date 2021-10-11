CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(255) primary key);
CREATE TABLE workflows (
  id bigint primary key unique not null,
  certs jsonb not null
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20211011095324');
