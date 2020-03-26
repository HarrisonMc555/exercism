#[macro_use]
extern crate lazy_static;

use regex::{Captures, Regex};
use std::collections::HashMap;

lazy_static! {
    static ref WORD_RE: Regex = Regex::new(r"\b((?P<word2>\w+'\w+)|'(?P<word3>\w+)'|(?P<word1>\w+))\b").unwrap();
}

/// Count occurrences of words.
pub fn word_count(words: &str) -> HashMap<String, u32> {
    let mut counts = HashMap::new();
    for word in WORD_RE
        .captures_iter(words)
        .filter_map(|c| extract_word(&c).and_then(clean_word))
    {
        let count = counts.entry(word).or_insert(0);
        *count += 1;
    }
    counts
}

fn extract_word<'a>(caps: &'a Captures) -> Option<&'a str> {
    caps.name("word1")
        .or_else(|| caps.name("word2"))
        .or_else(|| caps.name("word3"))
        .map(|c| c.as_str())
}

fn clean_word(word: &str) -> Option<String> {
    if word.is_empty() {
        None
    } else {
        Some(word.to_lowercase())
    }
}
