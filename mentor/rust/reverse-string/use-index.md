Great job! You've gotten a working solution in Rust—no small feat!

You're doing a great job of iterating through the indices and using each index to get a character out of the string. However, there's actually a much easier way to do this.

Because the result of the `chars` method implements the `Iterator` trait, you can use it in for loops (and much more). For example, you can iterate through the `chars` and call `push` every time.

Do you want to try to use the for loop over the iterator? It should be a lot simpler than directly indexing into the string.

Example:

```rust
for c in input.chars() {
    // Do something with char c
}
```