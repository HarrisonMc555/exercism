Great solution! Very nicely done. And great job using the `unicode-segmentation` crate!

I just have two suggestions. Feel free to move on and come back, or you can try them out here and I'll re-review your submission.

My first hint would be to remove the call to `flat_map(|g| g.chars())`. You can actually call `collect` on an iterator of `&str`s (which is what the `graphemes` method returns) to create a `String`.

If you want to know all of the kinds of iterators you can use to create a `String`, you can look [here] to see all of the types that `String` implements `FromIterator` for.

My second hint would be to try out [implicit return statements]! They're considered idiomatic Rust and help reduce redundancy.

Great job! Looking forward to your next solution!

[here]: https://doc.rust-lang.org/std/string/struct.String.html#impl-FromIterator%3C%26%27a%20char%3E
[implicit return statements]: https://doc.rust-lang.org/book/ch03-03-how-functions-work.html#functions-with-return-values