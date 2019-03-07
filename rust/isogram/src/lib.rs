use std::collections::HashSet;

pub fn check(candidate: &str) -> bool {
    let letters: Vec<_> = candidate
        .to_lowercase()
        .chars()
        .filter(|c| c.is_alphabetic())
        .collect();
    let unique_letters: HashSet<_> = letters.iter().collect();
    letters.len() == unique_letters.len()
}
