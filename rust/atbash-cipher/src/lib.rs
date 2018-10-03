const ATBASH_LENGTH: u8 = 26;
const ATBASH_START_CHAR: char = 'a';

/// "Encipher" with the Atbash cipher.
pub fn encode(plain: &str) -> String {
    atbash(plain)
    // unimplemented!("Encoding of {:?} in Atbash cipher.", plain);
}

/// "Decipher" with the Atbash cipher.
pub fn decode(cipher: &str) -> String {
    atbash(cipher)
    // unimplemented!("Decoding of {:?} in Atbash cipher.", cipher);
}

fn atbash(string: &str) -> String {
    string.chars().map(switch_char).collect()
}

fn switch_char(c: char) -> char {
    if c.is_ascii() {
        let letter_index = to_letter_index(c);
        let switched_letter_index = ATBASH_LENGTH - 1 - letter_index;
        from_letter_index(switched_letter_index)
    } else {
        c
    }
}

fn to_letter_index(c: char) -> u8 {
    to_u8(c.to_ascii_lowercase()) - to_u8(ATBASH_START_CHAR)
}

fn from_letter_index(index: u8) -> char {
    from_u8(index + to_u8(ATBASH_START_CHAR))
}

fn to_u8(c: char) -> u8 {
    c as u8
}

fn from_u8(u: u8) -> char {
    u as char
}
