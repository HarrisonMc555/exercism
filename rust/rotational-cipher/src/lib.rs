use lazy_static::lazy_static;

pub fn rotate(input: &str, key: i8) -> String {
    input.chars().map(|c| rotate_char(c, key)).collect()
}

lazy_static! {
    static ref UPPERCASE_LETTERS: Vec<char> = {
        (b'A'..=b'Z').map(char::from).collect()
    };
    static ref LOWERCASE_LETTERS: Vec<char> = {
        (b'a'..=b'z').map(char::from).collect()
    };
}

const NUM_LETTERS: u16 = 26;
const START_LOWERCASE: u16 = b'a' as u16;
const START_UPPERCASE: u16 = b'A' as u16;
fn rotate_char(c: char, key: i8) -> char {
    let key = key as i16 % NUM_LETTERS as i16;
    if c.is_ascii_lowercase() {
        let c = c as u16;
        let offset = (c - START_LOWERCASE) as i16;
        let new_offset = (offset + key) as u16 % NUM_LETTERS;
        let new_c = START_LOWERCASE + new_offset;
        new_c as u8 as char
    } else if c.is_ascii_uppercase() {
        let c = c as u16;
        let offset = (c - START_UPPERCASE) as i16;
        let new_offset = (offset + key) as u16 % NUM_LETTERS;
        let new_c = START_UPPERCASE + new_offset;
        new_c as u8 as char
    } else {
        c
    }
}
