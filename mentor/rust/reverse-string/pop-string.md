Nice work! Good job figuring out how to get a working solution in a new language. Rust also has some pretty strong opinions on mutation and strings, so great job working through that!

I see that you managed to go through the original string in reverse order by repeatedly calling `pop`. This is a totally reasonable approach but has a couple of drawbacks. First, it forces us to make a copy of the string (since we need to mutate it). Second, it forces us to remove those characters each time we call `pop`. However, we don't really need a copy and we don't really need to remove the characters—we just want to copy the characters into a new string!

There are few tools that `str` provides us with that can let us do that. First, since we want to iterate through all of the characters in the string, we can use the [`chars`] method. However, we want to go through them in reverse order. How do we do that?

Fortunately, the `Chars` struct we get back from the `chars` method implements the [`Iterator`] trait. Why don't you take a look through the documentation for [`Iterator`] and see if there's anything there that would be useful? I think you'll find a lot of great tools you can use for this exercise and future exercises.

Good luck! Let me know if you get stuck.

[`chars`]: https://doc.rust-lang.org/std/primitive.str.html#method.chars
[`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html