In Rust, we typically prefer to avoid `as` casts whenever possible. In this case, using a floating-point number (e.g. `1e9`) is less preferable than an explicit integer `1_000_000`. This is because using `as` casting can have unexpected consequences, including loss of precision.