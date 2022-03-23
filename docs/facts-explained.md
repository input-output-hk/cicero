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

## The <span style="color:blue">value</span> field is the actual input provided to Actions:

The json can consist of an arbitrary number of fields, which can later be accessed when running an Action.

In contrary the binary_hash/binary fields of the db table represent the output which was generated for a specific Fact.

### A: empty, just for triggering a run once behaviour
```
{
	"examples/runners/bash":
	{
	}
}
```

### B: json description for external Actions
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

## There are currently two ways of storing binary artifacts while creating/updating a fact

**Note:** It's not ideal to do this if you want to generate output use **std.postFact** in an Action instead.

### Json of fact piped with binary data
```
echo -n '{"foo": 1}' | cat - /tmp/testfile | http :8080/api/fact
```

### Multipart Request: first part json of fact, second part binary artifact:
```
http --multipart :8080/api/fact first='{"foo": 1}' second@/tmp/testfile

```

## Use std.postFact to store output of an Action as binary artifact:

The intended way of storing a resulting binary artifact is to create it in an Action with the help of the **std.postFact** library.

### Have a deeper look at the [post-fact](https://github.com/input-output-hk/cicero/blob/main/actions/examples/post-fact.nix) example action.
