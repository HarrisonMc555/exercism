use counter::Counter;
use std::collections::HashSet;

pub fn anagrams_for<'a>(
    word: &str,
    possible_anagrams: &'a [&str],
) -> HashSet<&'a str> {
    let word = word.to_lowercase();
    let word_counter = letter_counter(&word);
    possible_anagrams
        .iter()
        .filter(|possibility| is_anagram(possibility, &word, &word_counter))
        .copied() // Just copying the reference
        .collect()
}

fn is_anagram(
    possibility: &str,
    word: &str,
    word_counter: &Counter<char>,
) -> bool {
    let possibility = possibility.to_lowercase();
    possibility != word && letter_counter(&possibility) == *word_counter
}

fn letter_counter(word: &str) -> Counter<char> {
    word.to_lowercase().chars().collect::<Counter<_>>()
}
