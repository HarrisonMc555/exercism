This looks great! Nice job figuring out how to use an external library.

In Rust, we typically like to use `const`ants whenever we can. In this situation, we can avoid performing the `pow` calculations every time by simply having one billion as a numeric literal. You can even make it a `const`ant! Read about that [here][const].

Do you want to try to do that before moving on? Other than that your solution looks great!

[const]: https://doc.rust-lang.org/std/keyword.const.html