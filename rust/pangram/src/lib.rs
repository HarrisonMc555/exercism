use std::collections::HashSet;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref LOWERCASE_LETTERS: Vec<char> =
        { (b'a'..=b'z').map(|b| b as char).collect() };
}

/// Determine whether a sentence is a pangram.
pub fn is_pangram(sentence: &str) -> bool {
    let sentence_letters: HashSet<_> =
        sentence.chars().map(|c| c.to_ascii_lowercase()).collect();
    LOWERCASE_LETTERS
        .iter()
        .all(|c| sentence_letters.contains(c))
}
