// the PhantomData instances in this file are just to stop compiler complaints
// about missing generics; feel free to remove them

use num::Integer;
use std::fmt::Display;

/// A Matcher is a single rule of fizzbuzz: given a function on T, should
/// a word be substituted in? If yes, which word?
pub struct Matcher<'a, T> {
    matcher: Box<Fn(&T) -> bool + 'a>,
    subs: Box<Fn(&T) -> String + 'a>,
}

impl<'a, T> Matcher<'a, T> {
    pub fn new<F, S>(matcher: F, subs: S) -> Matcher<'a, T>
    where
        F: Fn(&T) -> bool + 'a,
        S: Fn(&T) -> String + 'a,
    {
        Matcher {
            matcher: Box::new(matcher),
            subs: Box::new(subs),
        }
    }

    pub fn apply(&self, value: &T) -> Option<String> {
        if (*self.matcher)(value) {
            Some((*self.subs)(value))
        } else {
            None
        }
    }
}

/// A Fizzy is a set of matchers, which may be applied to an iterator.
///
/// Strictly speaking, it's usually more idiomatic to use `iter.map()` than to
/// consume an iterator with an `apply` method. Given a Fizzy instance, it's
/// pretty straightforward to construct a closure which applies it to all
/// elements of the iterator. However, we're using the `apply` pattern
/// here because it's a simpler interface for students to implement.
///
/// Also, it's a good excuse to try out using impl trait.
pub struct Fizzy<'a, T>
where
    T: Display
{
    matchers: Vec<Matcher<'a, T>>,
}

impl<'a, T> Fizzy<'a, T>
where
    T: Display
{
    pub fn new() -> Self {
        Fizzy {
            matchers: Vec::new(),
        }
    }

    pub fn add_matcher(mut self, matcher: Matcher<'a, T>) -> Self {
        self.matchers.push(matcher);
        self
    }

    /// map this fizzy onto every element of an interator, returning a new iterator
    pub fn apply<I: 'a>(&self, iter: I) -> impl Iterator<Item = String>
    where
        I: Iterator<Item = T>,
    {
        iter.map(move |value| self.apply_one(&value))
    }

    fn apply_one(&self, value: &T) -> String {
        let strings = self
            .matchers
            .iter()
            .filter_map(|matcher| matcher.apply(value))
            .collect::<Vec<String>>();
        if strings.is_empty() {
            value.to_string()
        } else {
            strings.join("")
        }
    }
}

/// convenience function: return a Fizzy which applies the standard fizz-buzz rules
pub fn fizz_buzz<T>() -> Fizzy<'static, T>
where
    T: Integer + Display
{
    let three = T::one() + T::one() + T::one();
    let five = T::one() + T::one() + T::one() + T::one() + T::one();
    let fizz = Matcher::new(move |x: &T| x.is_multiple_of(&three), |_| "fizz".to_string());
    let buzz = Matcher::new(move |x: &T| x.is_multiple_of(&five), |_| "buzz".to_string());
    Fizzy::new().add_matcher(fizz).add_matcher(buzz)
}
