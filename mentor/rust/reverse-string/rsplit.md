This is great! Excellent use of iterators, `collect`, etc.

I just have a few things I think you should look into before you move on.

First, using `rsplit` works but is a bit much for this exercise. It's usually used for splitting on patterns. A simpler solution to understand (and probably more performant, though I'm not sure) would be to use the [`rev`] and [`chars`] methods!

Great job! Looking forward to your next iteration 😊

[`chars`]: https://doc.rust-lang.org/std/primitive.str.html#method.chars
[`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev