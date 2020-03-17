Great job! I like how you used iterators and correctly handled characters (not just bytes!).

This is probably close to idiomatic code in many other languages. However, in Rust, we prefer to avoid explicit `for` loops in simplie situations like this.

Try looking into the [`collect`] method! I think you'll find it quite useful :-)

If you're ok with it, let's try to do that before moving on. It's such a common concept, I think you'll want to learn how to use it before moving forward.

[`collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect