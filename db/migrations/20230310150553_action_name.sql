-- migrate:up

ALTER TABLE action_active
RENAME TO action_name;

ALTER INDEX action_active_pkey
RENAME TO action_name_pkey;

ALTER TABLE action_name
ADD active bool DEFAULT false NOT NULL,
ADD private bool DEFAULT false NOT NULL;

UPDATE action_name
SET active = true;

CREATE OR REPLACE FUNCTION action_insert_trigger()
RETURNS TRIGGER
LANGUAGE PLPGSQL
AS $$
	BEGIN
		INSERT INTO action_name (name, active)
		VALUES (NEW.name, true)
		ON CONFLICT (name) DO UPDATE SET active = EXCLUDED.active;

		RETURN NULL;
	END;
$$;

CREATE OR REPLACE TRIGGER action_insert
AFTER INSERT ON action
FOR EACH ROW EXECUTE PROCEDURE action_insert_trigger();

CREATE OR REPLACE FUNCTION action_delete_trigger()
RETURNS TRIGGER
LANGUAGE PLPGSQL
AS $$
	BEGIN
		DELETE FROM action_name
		WHERE NOT EXISTS (
			SELECT NULL
			FROM action
			WHERE name = action_name.name
		);

		RETURN NULL;
	END;
$$;

CREATE OR REPLACE TRIGGER action_delete
AFTER DELETE ON action
FOR EACH STATEMENT EXECUTE PROCEDURE action_delete_trigger();

-- migrate:down

CREATE OR REPLACE FUNCTION action_insert_trigger()
RETURNS TRIGGER
LANGUAGE PLPGSQL
AS $$
	BEGIN
		INSERT INTO action_active (name)
		VALUES (NEW.name)
		ON CONFLICT DO NOTHING;

		RETURN NULL;
	END;
$$;

CREATE OR REPLACE TRIGGER action_insert
AFTER INSERT ON action
FOR EACH ROW EXECUTE PROCEDURE action_insert_trigger();

CREATE OR REPLACE FUNCTION action_delete_trigger()
RETURNS TRIGGER
LANGUAGE PLPGSQL
AS $$
	BEGIN
		DELETE FROM action_active
		WHERE NOT EXISTS (
			SELECT NULL
			FROM action
			WHERE name = action_active.name
		);

		RETURN NULL;
	END;
$$;

CREATE OR REPLACE TRIGGER action_delete
AFTER DELETE ON action
FOR EACH STATEMENT EXECUTE PROCEDURE action_delete_trigger();

ALTER TABLE action_name
DROP active,
DROP private;

ALTER INDEX action_name_pkey
RENAME TO action_active_pkey;

ALTER TABLE action_name
RENAME TO action_active;
