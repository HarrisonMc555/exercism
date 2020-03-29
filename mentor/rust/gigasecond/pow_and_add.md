This is a great solution! Good job figuring out how to use the `chrono` library.

The `Add` trait is provided by `std::ops` so that external custom types can be "added" together. However, we typically don't call the `add` method directly, instead, we use the `+` operator. This operator is syntactic sugar for calling the `add` method but is a little more intuitive.

In addition, in Rust, we typically like to use `const`ants whenever we can. In this situation, we can avoid performing the `pow` calculations every time by simply having one billion as a numeric literal. You can even make it a `const`ant! Read about that [here][const].

Do you want to try to do those things before moving on? Other than that your solution looks great!

[const]: https://doc.rust-lang.org/std/keyword.const.html