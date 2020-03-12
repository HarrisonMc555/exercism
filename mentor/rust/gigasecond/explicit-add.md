This is a great solution! Good job figuring out how to use the `chrono` library.

The `Add` trait is provided by `std::ops` so that external custom types can be "added" together. However, we typically don't call the `add` method directly, instead, we use the `+` operator. This operator is syntactic sugar for calling the `add` method but is a little more intuitive.

Do you want to try that out before moving on? I think it's a good example of operator overloaindg in Rust.