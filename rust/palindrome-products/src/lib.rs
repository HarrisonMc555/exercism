#![allow(clippy::redundant_closure)]

pub fn get_palindrome_products(min: u64, max: u64) -> (u64, u64) {
    (min, max)
}

pub fn min(palindromes: &(u64, u64)) -> Option<u64> {
    let &(min, max) = palindromes;
    for end in min..=max {
        let xs = min..=end;
        let ys = (min..=end).rev();
        let pairs = xs.zip(ys).take_while(|(x, y)| x <= y);
        let products = pairs.map(|(x, y)| x * y);
        let palindromes = products.filter(|product| is_palindrome(product));
        if let Some(product) = palindromes.min() {
            return Some(product);
        }
    }
    None
}

pub fn max(palindromes: &(u64, u64)) -> Option<u64> {
    let &(min, max) = palindromes;
    for start in (min..=max).rev() {
        let xs = start..=max;
        let ys = (min..=max).rev();
        let pairs = xs.zip(ys).take_while(|(x, y)| x <= y);
        let products = pairs.map(|(x, y)| x * y);
        let palindromes = products.filter(|product| is_palindrome(product));
        if let Some(product) = palindromes.max() {
            return Some(product);
        }
    }
    None
}

fn is_palindrome<T: std::fmt::Display>(value: T) -> bool {
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
