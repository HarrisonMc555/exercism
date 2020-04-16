This is great! Excellent use of iterators, implicit returns, `collect`, etc.

The only thing I would point out is that you're calling `to_string` after calling `collect`. The `to_string` function takes a string slice (`&str`) and returns an owned `String` by making a copy of it. However, the `collect` function already returned a `String`! So the call to `to_string` is making an unnecessary second copy.

That's a minor issue, though. Great job! Good luck with future exercises.