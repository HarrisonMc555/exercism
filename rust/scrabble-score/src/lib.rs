#[macro_use] extern crate lazy_static;

use std::collections::HashMap;

const SCRABBLE_SCORE_TO_LETTER_PAIRS: [(u64, &[char]); 7] = [
    (1,  &['a', 'e', 'i', 'o', 'u', 'l', 'n', 'r', 's', 't']),
    (2,  &['d', 'g']),
    (3,  &['b', 'c', 'm', 'p']),
    (4,  &['f', 'h', 'v', 'w', 'y']),
    (5,  &['k']),
    (8,  &['j', 'x']),
    (10, &['q', 'z']),
];

lazy_static!{
    static ref SCRABBLE_SCORE_TO_LETTER_HASH: HashMap<u64, &'static [char]> =
        SCRABBLE_SCORE_TO_LETTER_PAIRS.iter().cloned().collect();

    static ref SCRABBLE_SCORE_HASH: HashMap<char, u64> = {
        let mut m = HashMap::new();
        for (&score, &letters) in SCRABBLE_SCORE_TO_LETTER_HASH.iter() {
            for &letter in letters {
                m.insert(letter, score);
            }
        }
        m
    };
}

/// Compute the Scrabble score for a word.
pub fn score(word: &str) -> u64 {
    word.chars()
        .map(letter_score)
        .sum()
}

fn letter_score(c: char) -> u64 {
    *SCRABBLE_SCORE_HASH.get(&c.to_ascii_lowercase()).unwrap_or(&0)
}
