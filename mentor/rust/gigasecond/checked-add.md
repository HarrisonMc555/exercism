This is a great solution! Good job figuring out how to use the `chrono` library.

The `checked_add_signed` method is most helpful when you have the chance to recover gracefully from an error. In this situation, there's not much we can do. The function signature returns a `DateTime<Utc>`, not an `Option<DateTime<Utc>>` or a `Result<DateTime<Utc>, _>`, so the only thing we can do is `panic`.

Since we're going to `panic` anyways, you may as well use the "unchecked" version of the "add" method. Conveniently, it falls under the implementation of the `std::ops::Add` trait, and thus is available as the `+` operator! So that's convenient.

Do you want to try that out before moving on? I think it's a good example of operator overloading in Rust.