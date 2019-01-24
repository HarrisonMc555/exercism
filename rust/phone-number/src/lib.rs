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
        && user_number[0] == '1'
}
