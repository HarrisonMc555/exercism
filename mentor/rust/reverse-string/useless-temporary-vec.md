This is great! Nice use of the `chars` iterator.

One thing to keep in mind is that every time you call `collect` you are allocating and creating an entirely new data structure. In this case, you are creating a `Vec<char>`. However, you don't actually need this `Vec`—you're just iterating over the contents.

As it turns out, a `Vec` isn't the only thing you can create using the `collect` method. You can create anything that implements the `FromIterator` trait for the iterator you have. This includes `HashMap`, `Vec`, and even...`String`! For an iterator of `char`s or `str`s, that is.

Try to see if you can get rid of the for loop and see how that affects your solution! Looking forward to your next iteration 😊

[`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev