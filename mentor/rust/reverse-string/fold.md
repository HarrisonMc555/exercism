I like your functional programming-esque solution! Do you come from a functional programming background?

Although there are times where a fold may still be useful in Rust, you'll probably see it a lot less often than in languages like Haskell. This is because the most common use of a fold is to construct a collection of some sort—which is exactly what we want to do here!

Because creating a collection from an iterator is so common, the [`Iterator`] trait provides the [`collect`] method. This method will create pretty much any collection you want from an iterator. This includes `Vec`s, `HashSet`s, and even `String`s!

There are a lot of other methods on [`Iterator`]. Why don't you check it out and see if there's anything that may prove useful to you for this exercise?

[`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html
[`collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect