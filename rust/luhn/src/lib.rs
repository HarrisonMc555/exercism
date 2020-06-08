/// Check a Luhn checksum.
extern crate num_integer;

use num_integer::Integer;

const RADIX: u32 = 10;
const LUHN_DIVISOR: u32 = 10;
const LUHN_MIN_LENGTH: usize = 2;

pub fn is_valid(code: &str) -> bool {
    let code = clean(code);
    if invalid_format(&code) {
        return false;
    }
    let digits = to_digits(&code);
    let second_digits_doubled = double_every_second_num_from_right(&digits);
    let sum: u32 = second_digits_doubled.iter().sum();
    sum % LUHN_DIVISOR == 0
}

fn double_every_second_num_from_right(code: &[u32]) -> Vec<u32> {
    let code_count_even = code.len().is_even();
    let is_second_from_right = |index: usize| index.is_even() == code_count_even;
    code.iter()
        .enumerate()
        .map(|(i, d)| {
            if is_second_from_right(i) {
                luhn_double(*d)
            } else {
                *d
            }
        })
        .collect()
}

fn luhn_double(num: u32) -> u32 {
    let doubled = num * 2;
    if doubled >= RADIX {
        doubled - (RADIX - 1)
    } else {
        doubled
    }
}

fn clean(code: &str) -> String {
    code.chars().into_iter().filter(|c| *c != ' ').collect()
}

fn valid_format(code: &str) -> bool {
    contains_valid_chars(code) && valid_length(code)
}

fn invalid_format(code: &str) -> bool {
    !valid_format(code)
}

fn contains_valid_chars(code: &str) -> bool {
    code.chars().all(|c| c.is_digit(RADIX))
}

fn valid_length(code: &str) -> bool {
    code.len() >= LUHN_MIN_LENGTH
}

fn to_digits(string: &str) -> Vec<u32> {
    string.chars().map(|c| c.to_digit(RADIX).unwrap()).collect()
}
