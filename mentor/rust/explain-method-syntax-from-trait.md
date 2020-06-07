Let's say you have a struct `Foo` like so:

```rust
struct Foo {}

impl Foo {
    fn method(&self, bar: i32) {
        // ...
    }
}
```

Note how `method` takes a reference to `self`. If you later call, this method, like so:

```rust
let foo = Foo {};
foo.method(42);
```

This is really just syntactic sugar for:

```rust
let foo = Foo {};
Foo::method(foo, 42);
```

At run-time, Rust only has function calls. Some of these function calls happen to take `self` (either by reference, mutable reference, or by ownership).

Note that in Rust, `self` is a reserved word and necessary. This is in contrast to Python, where `self` is the name of the object parameter by convention only.

The unique thing about Rust is that in addition to defining an `impl` for a type and including "methods" that take `self`, you can also define **traits** that `self`.

For example:

```rust
trait Secret {
    fn get_secret(&self) -> i32;
}
````

If you then implement that trait for a type, you can now call that method on that type. Since it takes `self` as a parameter, you can use method syntax just like you would with any other "method".

```rust
impl Wonderful for Foo {
    fn get_secret(&self) -> i32 {
        42
    }
}

fn main() {
    let foo = Foo {};
    println!("Secret: {}", foo.secret());
}
```

Does that help?