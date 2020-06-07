This is great! Excellent use of iterators, `collect`, etc.

My only tip would be to remove the call to `into_iter`. As it turns out, you don't need it! The `into_iter` method is great when you have something that *isn't* an iterator but you want to create an iterator out of it (e.g. a `Vec`). In this case, however, you already *have* an iterator (the result of the `chars` method is an iterator).

However, this solution is great as is. Good luck with future exercises!

P.S. If you're interested in learning more about how complex strings can be, you can run the "bonus" test case with `cargo test --all-features`. Feel free to move on and come back though.