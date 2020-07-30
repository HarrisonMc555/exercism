const ATBASH_LENGTH: u8 = 26;
const ATBASH_START: u8 = b'a';
const ATBASH_WORD_LENGTH: usize = 5;

/// "Encipher" with the Atbash cipher.
pub fn encode(plain: &str) -> String {
    atbash(plain, true)
}

/// "Decipher" with the Atbash cipher.
pub fn decode(cipher: &str) -> String {
    atbash(cipher, false)
}

fn atbash(string: &str, add_spaces: bool) -> String {
    let converted: Vec<_> = string
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .map(switch_char)
        .collect();
    if !add_spaces {
        return converted.iter().collect();
    }
    let with_spaces: Vec<char> = insert_between(&converted, &[' '], ATBASH_WORD_LENGTH)
        .into_iter()
        .collect();
    let without_trailing_space = &with_spaces[..with_spaces.len() - 1];
    without_trailing_space.iter().collect()
}

fn switch_char(c: char) -> char {
    if c.is_ascii_alphabetic() {
        switch_letter(c)
    } else {
        c
    }
}

fn insert_between<T>(arr: &[T], arr_between: &[T], num_between: usize) -> Vec<T>
where
    T: Copy,
{
    arr.chunks(num_between)
        .flat_map(|chunk| {
            let mut v = chunk.to_vec();
            v.extend(arr_between);
            v
        })
        .collect()
}

fn switch_letter(c: char) -> char {
    let letter_index = c.to_ascii_lowercase() as u8 - ATBASH_START;
    let switched_letter_index = ATBASH_LENGTH - 1 - letter_index;
    (switched_letter_index + ATBASH_START) as char
}
