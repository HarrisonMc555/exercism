Great job! Excellent use of iterators.

Good job discovering the `from_iter` method! However, it's typically considered more idiomatic to use the [`collect`] method instead. It's also included by default, so you don't need to import anything special.

Do you want to try that before moving on? The `collect` method is extremely common in Rust code, so I think it would be worth looking into before moving on.

[`collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect