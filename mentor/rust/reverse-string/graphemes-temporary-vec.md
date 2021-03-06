This is great! Very nice use of the `unicode_segmentation` crate. This solution shows that you've mastered several Rust concepts.

One thing to keep in mind is that every time you call `collect` you are allocating and creating an entirely new data structure. In this case, you are creating a `Vec<&str>`. However, you don't actually need this `Vec`—in the end, you want a `String`.

Sometimes, allocating temporary data structures is necessary for certain algorithms or optimizations. However, in many situations, you can avoid these intermediary structures.

In this case, the only thing you use the `Vec` for is to reverse it. However, there is a [`rev`] method defined on iterators!

Even if this doesn't end up making the code more efficient it's still considered idiomatic Rust code (and makes it much easier to read).

Try using that and see how it affects your solution! Looking forward to your next iteration 😊

[`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev