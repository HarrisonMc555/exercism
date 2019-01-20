const BASE: u32 = 10;
const ISBN_LENGTH: usize = 10;

/// Determines whether the supplied string is a valid ISBN number
pub fn is_valid_isbn(isbn: &str) -> bool {
    let isbn = clean(isbn);
    let digits = match get_digits(&isbn) {
        Some(digits) => digits,
        None => return false,
    };
    let sum: u32 = multiply_digits(&digits).iter().sum();
    println!("sum: {}", sum);
    sum % 11 == 0
}

fn clean(isbn: &str) -> Vec<char> {
    isbn.chars().filter(char::is_ascii_alphanumeric).collect()
}

fn get_digits(isbn: &[char]) -> Option<Vec<u32>> {
    if isbn.len() != ISBN_LENGTH {
        return None;
    }
    let digits: Option<Vec<u32>> = isbn[..isbn.len() - 1]
        .iter()
        .map(|c| c.to_digit(BASE))
        .collect();
    let check = check_digit(isbn[isbn.len() - 1]);
    combine_options(digits, check, |mut ds, c| {
        ds.push(c);
        ds
    })
}

fn check_digit(check_char: char) -> Option<u32> {
    if let Some(x) = check_char.to_digit(BASE) {
        return Some(x);
    }
    if check_char == 'X' {
        return Some(BASE as u32);
    }
    None
}

fn multiply_digits(digits: &[u32]) -> Vec<u32> {
    assert!(digits.len() == ISBN_LENGTH);
    digits
        .iter()
        .zip((1..=ISBN_LENGTH).rev())
        .map(|(&digit, factor)| digit * factor as u32)
        .collect()
}

fn combine_options<T, U, V, F>(
    a: Option<T>,
    b: Option<U>,
    combine: F,
) -> Option<V>
where
    F: FnOnce(T, U) -> V,
{
    match (a, b) {
        (Some(a), Some(b)) => Some(combine(a, b)),
        _ => None,
    }
}
