This is great! Excellent use of iterators, `collect`, etc.

I just have a few things I think you should look into before you move on.

First, using `rsplit` works but is a bit overkill. I would try looking into the [`rev`] and [`chars`] methods!

In addition, you're using the `collect` method to turn an iterator of strings into a `Vec` of strings. You then use this `Vec` of strings to join them together into one final string. However, this temporary `Vec` is actually unnecessary! You could actually just `collect` into a `String` directly.

Great job! Looking forward to your next iteration 😊

[`chars`]: https://doc.rust-lang.org/std/primitive.str.html#method.chars
[`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev