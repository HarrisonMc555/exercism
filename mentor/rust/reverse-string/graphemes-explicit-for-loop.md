Good job using the `unicode_segmentation` crate! Very nicely done.

This is probably close to idiomatic code in many other languages. However, in Rust, we prefer to use `iterator` combinations instead of explicit for loops.

Try looking into the [`collect`] method. I think you'll find it quite useful 😊

If you're ok with it, let's try to do that before moving on. It's such a common concept, I think you'll want to learn how to use it before moving forward.

[`collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect