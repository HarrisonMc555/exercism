const PHONE_NUMBER_LENGTH: usize = 10;

pub fn number(user_number: &str) -> Option<String> {
    let cleaned = clean(user_number);
    get_number(&cleaned).map(|chars| chars.iter().collect())
}

fn clean(user_number: &str) -> Vec<char> {
    user_number.chars().filter(char::is_ascii_digit).collect()
}

fn get_number(user_number: &[char]) -> Option<&[char]> {
    get_normal(user_number).or_else(|| get_usa(user_number))
}

fn get_normal(user_number: &[char]) -> Option<&[char]> {
    if is_valid_normal(user_number) {
        Some(user_number)
    } else {
        None
    }
}

fn is_valid_normal(user_number: &[char]) -> bool {
    user_number.len() == PHONE_NUMBER_LENGTH
        && is_valid_area_code(&user_number[0..3])
        && is_valid_exchange_code(&user_number[3..6])
}

fn get_usa(user_number: &[char]) -> Option<&[char]> {
    if is_valid_usa(user_number) {
        Some(&user_number[1..])
    } else {
        None
    }
}

fn is_valid_usa(user_number: &[char]) -> bool {
    user_number.len() == PHONE_NUMBER_LENGTH + 1
        && is_usa_country_code(user_number[0])
        && is_valid_area_code(&user_number[1..4])
        && is_valid_exchange_code(&user_number[4..7])
}

const USA_COUNTRY_CODE: char = '1';

fn is_usa_country_code(country_code: char) -> bool {
    country_code == USA_COUNTRY_CODE
}

const INVALID_LEADING_AREA_CODE_DIGITS: [char; 2] = ['0', '1'];

fn is_valid_area_code(area_code: &[char]) -> bool {
    let leading_area_code_digit = area_code[0];
    let invalid = INVALID_LEADING_AREA_CODE_DIGITS
        .iter()
        .any(|&c| c == leading_area_code_digit);
    !invalid
}

const INVALID_LEADING_EXCHANGE_CODE_DIGITS: [char; 2] = ['0', '1'];

fn is_valid_exchange_code(exchange_code: &[char]) -> bool {
    let leading_exchange_code_digit = exchange_code[0];
    let invalid = INVALID_LEADING_EXCHANGE_CODE_DIGITS
        .iter()
        .any(|&c| c == leading_exchange_code_digit);
    !invalid
}
