This is great! Excellent use of iterators, implicit returns, `collect`, etc. Also, nice work using the `unicode_segmentation` crate.

Before you move on, I just want to point out a couple of things.

If you look at the definition of the [`graphemes`] method, you'll notice that the first argument is `&self` (we can ignore the `'a`, it's irrelevant to this discussion). This means that we can call this function with "method" syntax, like so:

```rust
my_string.graphemes(is_extended)
```

This is typically preferred over the "fully-qualified" syntax:

```rust
UnicodeSegmentation::graphemes(my_string, is_extended)
```

It's not a big deal, but I think you'll find that it helps make your code easier to read!

Good luck with the next exercise! Let me know if you have any questions.

[`graphemes`]: https://docs.rs/unicode-segmentation/1.6.0/unicode_segmentation/trait.UnicodeSegmentation.html#tymethod.graphemes