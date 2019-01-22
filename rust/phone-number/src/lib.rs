pub fn number(user_number: &str) -> Option<String> {
    Some(user_number.chars().filter(char::is_ascii_digit).collect())
}
