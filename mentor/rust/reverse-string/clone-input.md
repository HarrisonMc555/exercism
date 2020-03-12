This is great! Excellent use of iterators, implicit returns, `collect`, etc.

The only thing I would point out is that you're making a copy of `input`. However, you don't actually need to do that, since the `chars` method is available on `&str`s as well as `String`s!

If you're interested in learning more about how complex strings can be, you can run the "bonus" test case with `cargo test —all-features`. Feel free to move on and come back though :-)

Great job!