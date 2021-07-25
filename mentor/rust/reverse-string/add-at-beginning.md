Great job! Excellent use of `chars` method.

As it turns out, your `for` loop is creating an entirely new string during every iteration. That's not a huge deal for small strings but would take a very long time if you were trying to reverse a very long string.

What if, instead of going through the original string and adding it to the beginning (which requires copying the whole string), we went through the original string in reverse order and added things to the end? We can avoid this by using the [`rev`] method! Try it out and see if that helps. Also, take a look at the other methods defined on [`Iterator`]! They're very useful.

[`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev
[`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html