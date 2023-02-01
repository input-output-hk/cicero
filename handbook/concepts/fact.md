# Facts

Facts are the blood of Cicero. Without them, nothing happens.

An action is triggered when all its inputs are satisfied by a fact
and may publish a fact as a result.

This is similar to event-driven systems. Facts are not just events though:

- An event is fired and then lost; facts are recorded and kept forever.
- An event describes, like the name says, an event that happened in the past;
	facts aim to describe the **state of the world** from the moment of the fact's inception on.
- An event cannot become too big; facts may have arbitrary binary data attached (called an artifact).

Now that we know what role facts play in Cicero, let's have a look at what they are comprised of in detail:

- An UUID.
- A value, which is plain JSON. This is used to match against inputs.
- The UUID of the run that published the fact, if any.
- The timestamp of their creation.
- Optionally arbitrarily-large binary data, called the [artifact](#artifacts).
- An SRI hash of the artifact, if any.

### Artifacts

Artifacts are simply the binary data that may be attached to a fact.

There can only be one blob attached or none; it is not possible for a fact to have multiple artifacts.
If you want to attach multiple files, use an archive format like tar.

A checksum of the artifact is also recorded in the fact in SRI format.

A fact that has an artifact attached may also itself be referred to as an artifact for simplicity.

Note that facts cannot be deleted without interrupting tracability
so you may not want to attach too huge artifacts.
