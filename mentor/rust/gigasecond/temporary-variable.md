This looks great! Nice job figuring out how to use an external library.

My only suggestion for "style" would be to remove the temporary variable. In general, named values help readability. However, in this case, we just end up immediately returning the value we created. Because of that, it doesn't really help much. In this case, it's considered idiomatic Rust to simply return the expression.

However, this solution is totally fine as it is. Good luck with future exercises!