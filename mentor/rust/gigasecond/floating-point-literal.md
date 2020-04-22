This looks great! Nice job figuring out how to use an external library.

One thing to keep in mind when dealing with numeric literals (like `1e9`) is what the data types are and where precision may be lost.

Whenever you're converting from a floating point number (e.g. `f32` or `f64`) to an integer number (`i32`, etc.), you will lose any fractional parts. For example, `1.23` will become just `1`. However, that's not the only case where precision may be lost! If you try to convert a sufficiently large number from a floating point number to a integer, or vice versa, you may overflow the range of the target type.

In this situation, it ends up being fine because floating point literals default to `f64` which can hold one billion. However, because there are so many tricky situations and "gotchas", it's common to avoid converting between the two when it's unnecessary.

In this situation, we really want an integer, so using a floating point literal isn't ideal. Although it requires typing out a few more zeros, it's considered good form to type out the integer numeric literal.

Here are a few more things to consider:
  - You can use a named [constant][const].
  - You can use underscores as the separator for thousands, millions, etc.

Do you want to try to do that before moving on? Other than that your solution looks great!

[const]: https://doc.rust-lang.org/std/keyword.const.html