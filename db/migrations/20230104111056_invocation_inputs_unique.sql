-- migrate:up

ALTER TABLE invocation_inputs
ADD UNIQUE (invocation_id, input_name);

-- migrate:down

ALTER TABLE invocation_inputs
DROP CONSTRAINT invocation_inputs_invocation_id_input_name_key
