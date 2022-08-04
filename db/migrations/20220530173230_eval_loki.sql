-- migrate:up

ALTER TABLE invocation
DROP eval_stdout,
DROP eval_stderr;
