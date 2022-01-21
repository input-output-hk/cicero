-- migrate:up

CREATE SCHEMA api;

GRANT USAGE ON SCHEMA api TO cicero_api;

CREATE VIEW api.current_action AS
SELECT DISTINCT ON (name) *
FROM public.action
ORDER BY name, created_at DESC;

CREATE VIEW api.artifact AS
SELECT *
FROM fact
WHERE "binary" IS NOT NULL;

CREATE FUNCTION api.input_select_all(paths jsonb)
RETURNS SETOF fact
LANGUAGE plpgsql AS $$
	DECLARE
		sql text := 'SELECT * FROM public.fact WHERE TRUE';
		parts text[];

		i int := 1;
		path jsonb;
		part text;
	BEGIN
		FOR path IN SELECT jsonb_array_elements(paths) LOOP
			sql := sql || ' AND jsonb_extract_path(value';

			FOR part IN SELECT jsonb_array_elements_text(path) LOOP
				sql := sql || ', $1[' || i || ']';
				i := i + 1;

				parts := parts || part;
			END LOOP;

			sql := sql || ') IS NOT NULL';
		END LOOP;

		RETURN QUERY EXECUTE sql USING parts;
		RETURN;
	END;
$$;

CREATE FUNCTION api.input_select_latest(paths jsonb)
RETURNS SETOF fact
LANGUAGE plpgsql AS $$
	BEGIN
		RETURN QUERY
		SELECT *
		FROM api.input_select_all(paths)
		ORDER BY created_at DESC
		FETCH FIRST ROW ONLY;
		RETURN;
	END;
$$;

-- migrate:down

DROP SCHEMA api CASCADE;
