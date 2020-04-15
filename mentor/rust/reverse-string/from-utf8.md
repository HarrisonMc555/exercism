Nice solution! I'm guessing you've realized by now that Rust handles strings a little differently than a lot of other languages!

This is a great solution, and is probably close to the "idiomatic" solution in other languages. I think there is some room for improvement, though!

One thing to keep in mind is that Rust really tries hard to prevent programmers from accidentally treating something as a valid string when it isn't. That's why `String::from_utf8` returns a `Result<String, ...>` instead of a raw `String`.

However, Rust also is good about providing lots of nice helpers so that we don't usually have to worry about that kind of thing. For example, you can use the [`chars`] method on strings to get the characters out of a string. Since we know that all strings are made up of valid characters, this will never fail. If we use that, we can avoid the call to `from_utf8` and unwrapping the result.

Another tool that can help us is the [`Iterator`] trait. Anything that implements this traits gets a ton of useful methods for free. The `Chars` struct returned by the `chars` method implements this trait. I would encourage you to look at some of the available methods and see if there are any that would help in this situation!

Good luck, and let me know if you get stuck!

[`chars`]: https://doc.rust-lang.org/std/primitive.str.html#method.chars
[`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html