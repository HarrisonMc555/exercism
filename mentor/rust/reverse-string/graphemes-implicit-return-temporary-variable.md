This is great! Excellent use of iterators, `collect`, etc. Also, nice work using the `unicode_segmentation` crate.

My only tip would be removing the temporary variable. Occasionally I'll find it useful to give a value a name even though I'll be returning it immediately, but in this case there's not really a great name to give. Considering this function is so short, it would be considered idiomatic to simply us an implicit return statement for the entire expression. It would look something like this:

```rust
pub fn reverse(input: &str) -> String {
    input.graphemes(true).rev().collect()
}
```

However, that's only a minor nitpick.

Great job! Good luck with the next exercise!

[implicit return statements]: https://doc.rust-lang.org/book/ch03-03-how-functions-work.html#functions-with-return-values