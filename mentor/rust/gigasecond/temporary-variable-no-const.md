This looks great! Nice job figuring out how to use an external library.

I have just two suggestions for "style".

The first would be to put the numeric literal in a `const`. I like how you used the underscores as separators, though!

The second would be to remove the temporary variable. In general, named values help readability. However, in this case, we just end up immediately returning the value we created. Because of that, it doesn't really help much. In this case, it's considered idiomatic Rust to simply return the expression.

However, this solution is totally fine as it is. Good luck with future exercises!