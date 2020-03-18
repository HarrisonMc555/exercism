This is great! Very nicely done.

I just have a couple of suggestions:
- We don't really need a temporary variable here since the function is so short.
- When using large literals like this, we typically use underscores (`_`) as delimiters to separate the thousands, millions, etc.
- Literals (besides common things like `0` or `1`) are typically made into named `const`ants.
- We typically avoid using `return` unless we're exiting early.
- If we ever see ourselves creating a variable and immediately returning it, we typically replace that with just the value (and an implicit return).

However, this solution looks great as it is. Good luck with future exercises!