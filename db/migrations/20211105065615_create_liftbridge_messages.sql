-- migrate:up

CREATE TABLE liftbridge_messages (
  id uuid PRIMARY KEY DEFAULT public.gen_random_uuid(),
  "offset" bigint NOT NULL,
  stream text NOT NULL,
  subject text NOT NULL,
  created_at timestamp NOT NULL,
  value jsonb NOT NULL
);

-- migrate:down

DROP TABLE IF EXISTS liftbridge_messages;
