This looks great! Very nice solution. Good luck with future exercises!

Just as a personal viewpoint, I think this would be a situation where it would be acceptable to just have one solution—the one that properly handles graphemes. Here, we're using the `graphemes` feature flag to enable/disable a test, which isn't super common (and maybe not idiomatic). In general, a feature would be used to provide different functionality or significantly impact performance in some way.

For example, the popular `rand` crate includes a `log` feature that will enable logging. It would add a decent amount of overhead (and extra dependencies) if you had logging included but didn't need it. So, using a feature to toggle it on or off is beneficial.

In this case, using graphemes is always correct and doesn't significantly affect performance. So, it would probably be ok to always include it.

Obviously it doesn't matter for this educational exercise, but I wanted to give you my point of view for when crates are useful/beneficial in the future. Hope that helps!