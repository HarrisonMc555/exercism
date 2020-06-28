P.S. Instead of writing out `chrono::Duration::seconds`, a common pattern is to import any types you want to use. In this case that is `chrono::Duration`. You can add it to the list of imported types like so:

```rust
use chrono::{DateTime, Duration, Utc};
```

Then, you can use `Duration` without qualifying it as `chrono::Duration`, like so:

```rust
start + Duration::seconds(...)
```