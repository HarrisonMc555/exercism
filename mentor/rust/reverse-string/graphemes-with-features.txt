This is great! Excellent use of iterators, implicit returns, `collect`, etc. Also, nice work using the `unicode_segmentation` crate.

I'm impressed that you partitioned your solutions depending on the `graphemes` feature! I didn't start using features for much longer in my Rust journey.

In this case, I think it would be fine to just provide the one unicode-aware solution. For strings that aren't too complex, the solution may be *slightly* slower, but still correct. That's just the way I lean, I could see it going both ways.

Either way, I'm glad you learned about features and graphemes and whatnot! Good luck with future exercises!