This is great! Excellent use of iterators, implicit returns, `collect`, etc. This is very easy to read.

There's one thing about this solution that makes it less performant than it otherwise would be. Calling `to_string` inside `map` means that you're calling the `to_string` function on every character of the string. However, you can actually call `collect` to create a string with an iterator of `char`s. So, if you just removed the `map` call it would actually still work!

Since every call to `to_string` creates a new allocated `String`, this is a fairly expensive call to put inside a loop (which is essentially what is happening here).

Do you want to try removing it and seeing if it still works?