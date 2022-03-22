# Facts explained

Facts are basically a way to store the input and output of a Run of a Cicero Action.

The db schema for a fact can be inspected like this:
```
nix develop
psqlc
cicero=# \d fact
                                   Table "public.fact"
   Column    |            Type             | Collation | Nullable |        Default
-------------+-----------------------------+-----------+----------+-----------------------
 id          | uuid                        |           | not null | gen_random_uuid()
 run_id      | uuid                        |           |          |
 value       | jsonb                       |           | not null |
 binary_hash | text                        |           |          |
 binary      | lo                          |           |          |
 created_at  | timestamp without time zone |           | not null | statement_timestamp()
```

The **value** field is the actual input which contains some valid json, which could look like:

# A: empty, just for triggering a run once behaviour
```
{
	"examples/runners/bash":
	{
	}
}
```

# B: json description for external Actions
```
{
	"examples/runners/bash":
	{
		"start":
		{
			"sha": "9ceba66ef56c1d6b5a9e366f384f3fe4fe6c2576",
			"clone_url": "https://github.com/input-output-hk/cicero/tree/main",
			"target_branch": "main"
		}
	}
}
```
