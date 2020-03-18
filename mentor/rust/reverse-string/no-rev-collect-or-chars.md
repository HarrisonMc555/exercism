This is great! It looks like you've learned a lot about how Rust works

One thing to keep in mind is that constructing new data structures costs time and memory. In this case, you are creating a `Vec<&str>`. However, you don't actually need this `Vec`—in the end, you want a `String`.

Sometimes, allocating temporary data structures is necessary for certain algorithms or optimizations. However, in many situations, you can avoid these intermediary structures.

In this case, the only thing you use the `Vec` for is to reverse it. However, there is a [`rev`] method defined on iterators!

Even if this doesn't end up making the code more efficient it's still considered idiomatic Rust code (and makes it much easier to read).

Also, the idiomatic way to get characters out of a string is the [`chars`] method, although `.split("")` works, too.

Try using those and see how that affects your solution! Looking forward to your next iteration :-)

[`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev
[`chars`]: https://doc.rust-lang.org/std/primitive.str.html#method.chars