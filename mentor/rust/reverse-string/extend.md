Great job! Excellent use of `chars` method.

Nice job finding and using the `extend` method. This method is especially useful when you have a pre-existing string and are adding to it. However, in our case, we're just trying to create a string from the reversed result of `chars`.

Fortunately, Rust has a built-in method that can take any iterator (including the result of the `chars` method) and create a collection out of it (e.g. `Vec`, `HashSet`, or even `String`!). This is the [`collect`] method.

If you're ok with it, let's try to use that before moving on. It's such a common method that I think you'll want to learn how to use it before moving forward.

[`collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect