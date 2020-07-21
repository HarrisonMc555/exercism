The way that traits work in Rust is a little interesting. There are three ways a trait can be implemented for a type.

1. A crate author defines both a trait and a type, and implements the trait for the type (e.g. `impl MyNewTrait for MyNewType`).
2. A crate author defines a type and implements an external trait for their type (e.g. `impl Display for MyNewType`).
3. A crate author defines a trait and implements that trait for an external type (e.g. `impl MyNewTrait for String`).