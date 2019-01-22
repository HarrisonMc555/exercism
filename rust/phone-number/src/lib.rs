const PHONE_NUMBER_LENGTH: usize = 10;

pub fn number(user_number: &str) -> Option<String> {
    let cleaned = clean(user_number);
    if is_valid(&cleaned) {
        Some(cleaned.iter().collect())
    } else {
        None
    }
}

fn clean(user_number: &str) -> Vec<char> {
    user_number.chars().filter(char::is_ascii_digit).collect()
}

fn is_valid(user: &[char]) -> bool {
    user.len() == PHONE_NUMBER_LENGTH
}
