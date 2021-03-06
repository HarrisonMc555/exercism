Nice solution! I'll admit, this crate didn't exist when we created this exercise—and I have a sneaking suspicion that our exercise may have sparked the creation of the crate! (No idea, really, though)

I think you probably learned some valuable lessons about using external libraries ("crates") in Rust. However, there are a few lessons we wanted students to learn about iterators, collections, etc. Would you be interested in trying to solve this exercise without the `unicode_reverse` crate? We typically recommend using the [`unicode_segmentation`] crate. It provides the tools to correctly handle Unicode graphemes without providing the answer whole-cloth.

Good luck on your next iteration! Let me know if you have any questions.

P.S. You may want to look into the [`collect`] method—it will be quite helpful!

[`collect`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect
[`unicode_segmentation`]: https://docs.rs/unicode-segmentation/1.6.0/unicode_segmentation