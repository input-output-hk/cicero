# Runs

A run is how Cicero keeps track of an action's [job](action.md#job) after it is scheduled.

After the job finished Cicero will publish the respective [output](output.md).

If the action is a [decision](action.md#decision), the run will end immediately and publish the [`success`](output.md#success) output.
