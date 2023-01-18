-- migrate:up

CREATE TABLE action_active (
	name text PRIMARY KEY
);

CREATE FUNCTION action_insert_trigger()
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

CREATE TRIGGER action_insert
AFTER INSERT ON action
FOR EACH ROW EXECUTE PROCEDURE action_insert_trigger();

CREATE FUNCTION action_delete_trigger()
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

CREATE TRIGGER action_delete
AFTER DELETE ON action
FOR EACH STATEMENT EXECUTE PROCEDURE action_delete_trigger();

INSERT INTO action_active
SELECT DISTINCT name
FROM action
WHERE active;

DROP VIEW api.current_action;

ALTER TABLE action
DROP active;

CREATE VIEW api.current_action AS
SELECT DISTINCT ON (name) *
FROM public.action
ORDER BY name, created_at DESC;
