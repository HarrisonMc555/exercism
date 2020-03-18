Great solution! Good job figuring out how to use an external library.

As it turns out, you don't actually need to convert to and from a timestamp—the `chrono` library provides a way to increment a `DateTime` by a `Duration`, which can be however long you want it to be (see [this][`DateTime::Add`]).

How about you try figuring how to use that functionality? If you have any questions (like how the `Add` trait works) feel free to reach out!

[`DateTime::Add`]: https://docs.rs/chrono/0.4.10/chrono/struct.DateTime.html#impl-Add%3CDuration%3E