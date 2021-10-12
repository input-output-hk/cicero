CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(255) primary key);
CREATE TABLE workflows (
  id integer primary key unique not null,
  name text not null,
  certs jsonb not null,
  created_at text not null,
  updated_at text not null
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20211011095324');
