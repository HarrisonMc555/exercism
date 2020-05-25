Great job! I like how you used iterators and correctly handled characters (not just bytes!).

The only thing I would point out is that you don't actually need to call `String::from` in this case. The `chars` method takes a reference, so you can provide it a `&str`. When you call `String::from`, it's allocating an entirely new string, which will increase the space and time required for the function.

Other than that, though, this solution looks great! Very nicely done. Good luck with future exercises!