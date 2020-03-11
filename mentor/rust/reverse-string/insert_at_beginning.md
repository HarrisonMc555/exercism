As it turns out, inserting at the beginning of a `String` (or a `Vec`) requires copying the previous contents over to make room for the new element. Since we're doing this in a loop, we end up doing a lot of extra copying!

We can avoid this by using the [`rev`] method! Try it out and see if that helps. Also, take a look at the other methods defined on [`Iterator`]! They're very useful.

[`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev
[`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html