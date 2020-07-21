Great job! I like how you used iterators and correctly handled characters (not just bytes!).

This is probably close to idiomatic code in many other languages. However, in Rust, we prefer to avoid explicit `for` loops in simple situations like this.

Try looking into the [`collect`] method! I think you'll find it quite useful 😊

If you're ok with it, let's try to do that before moving on. It's such a common concept, I think you'll want to learn how to use it before moving forward.

P.S. Here's just a few more tips for idiomatic Rust code (but aren't big deals)

1. Even if you did need to use an explicit for loop, in this case you don't need to use the `enumerate` function—we don't really care about the indices of the characters.
2. Typically when creating a new empty string (or new empty collection of any kind) we'll use the `new` function (e.g. `String::new()`).
3. Typically we use [implicit return statements] to return values at the end of a function.

[`collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect
[implicit return statements]: https://doc.rust-lang.org/book/ch03-03-how-functions-work.html#functions-with-return-values