This is great! Excellent use of iterators, `collect`, etc. Also, nice work using the `unicode_segmentation` crate.

This is a great solution! You were able to reverse everything with only a single temporary `Vec`tor.

...what would you think if I told you it was possible *without* the temporary vector? 😊

Since reversing things is a relatively common task, Rust provides a [`rev`] method that is available on most iterators.

Do you want to try to use that and see how your solution looks afterwards? I think you should be able to greatly simplify it.

P.S. If you ever need to reverse something *in-place*, this would be a good algorithm to use. There's the [`reverse`] method on `Vec`, however, which should do what you need.

[`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev
[`reverse`]: https://doc.rust-lang.org/std/vec/struct.Vec.html#method.reverse