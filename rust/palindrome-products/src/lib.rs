use std::fmt;

pub type Palindrome = u64;

pub fn get_palindrome_products(min: u64, max: u64) -> Vec<Palindrome> {
    (min..=max)
        .enumerate()
        .flat_map(|(i, x)| (min..=max).skip(i).map(move |y| (x, y)))
        .map(|(x, y)| x * y)
        .filter(|product| is_palindrome(product))
        .collect()
}

pub fn min(palindromes: &[Palindrome]) -> Option<Palindrome> {
    palindromes.into_iter().min().cloned()
}

pub fn max(palindromes: &[Palindrome]) -> Option<Palindrome> {
    palindromes.into_iter().max().cloned()
}

fn is_palindrome<T: fmt::Display>(value: T) -> bool {
    let chars: Vec<_> = value.to_string().chars().collect();
    let num_chars = chars.len();
    for (i, &c) in chars.iter().take(num_chars / 2).enumerate() {
        let opposite_i = num_chars - i - 1;
        if c != chars[opposite_i] {
            return false;
        }
    }
    true
}
