use std::fmt::Display;
use std::ops::{Add, Rem};

/// A Matcher is a single rule of fizzbuzz: given a function on T, should
/// a word be substituted in? If yes, which word?
pub struct Matcher<T> {
    predicate: Box<dyn Fn(T) -> bool>,
    replacement: String,
}

impl<T> Matcher<T> {
    pub fn new<P>(predicate: P, replacement: &str) -> Matcher<T>
    where
        P: Fn(T) -> bool + 'static,
    {
        Matcher {
            predicate: Box::new(predicate),
            replacement: replacement.to_string(),
        }
    }

    pub fn apply(&self, value: T) -> Option<String> {
        if (self.predicate)(value) {
            Some(self.replacement.clone())
        } else {
            None
        }
    }
}

/// A Fizzy is a set of matchers, which may be applied to an iterator.
#[derive(Default)]
pub struct Fizzy<T>(Vec<Matcher<T>>);

impl<T> Fizzy<T> {
    pub fn new() -> Self {
        Fizzy(Vec::new())
    }

    pub fn add_matcher(mut self, matcher: Matcher<T>) -> Self {
        self.0.push(matcher);
        self
    }

    /// map this fizzy onto every element of an iterator, returning a new iterator
    pub fn apply<I>(self, iter: I) -> impl Iterator<Item = String>
    where
        I: IntoIterator<Item = T>,
        T: Clone + Display + 'static,
    {
        iter.into_iter().map(move |value| self.apply_one(value))
    }

    fn apply_one(&self, value: T) -> String
    where
        T: Clone + Display,
    {
        let strings = self
            .0
            .iter()
            .filter_map(|m| m.apply(value.clone()))
            .collect::<Vec<String>>();
        if strings.is_empty() {
            value.to_string()
        } else {
            strings.join("")
        }
    }
}

/// convenience function: return a Fizzy which applies the standard fizz-buzz rules
pub fn fizz_buzz<T>() -> Fizzy<T>
where
    T: Clone + Display + PartialEq + From<u8> + Add<Output = T> + Rem<Output = T> + 'static,
{
    let fizz = Matcher::new(move |x| is_multiple(x, 3.into()), "fizz");
    let buzz = Matcher::new(move |x| is_multiple(x, 5.into()), "buzz");
    Fizzy::new().add_matcher(fizz).add_matcher(buzz)
}

fn is_multiple<T>(num: T, divisor: T) -> bool
where
    T: PartialEq + From<u8> + Add<Output = T> + Rem<Output = T>,
{
    num % divisor == 0.into()
}
