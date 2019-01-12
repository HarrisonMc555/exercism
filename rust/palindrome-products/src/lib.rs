pub fn get_palindrome_products(min: u64, max: u64) -> (u64, u64) {
    (min, max)
}

pub fn min(palindromes: &(u64, u64)) -> Option<u64> {
    let &(min, max) = palindromes;
    (min..=max)
        .enumerate()
        .flat_map(|(i, x)| (min + i as u64..=max).map(move |y| (x, y)))
        .map(|(x, y)| x * y)
        .filter(|product| is_palindrome(product))
        .min()
}

pub fn max(palindromes: &(u64, u64)) -> Option<u64> {
    let &(min, max) = palindromes;
    (min..=max)
        .enumerate()
        .flat_map(|(i, x)| (min + i as u64..=max).map(move |y| (x, y)))
        .map(|(x, y)| x * y)
        .filter(|product| is_palindrome(product))
        .max()
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
