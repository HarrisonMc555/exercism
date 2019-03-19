pub fn encode(key: &str, message: &str) -> Option<String> {
    Some(message
        .chars()
        .zip(key.chars().cycle())
        .map(|(mc, kc)| encode_char(kc, mc))
        .collect::<Option<Vec<_>>>()?
        .iter()
        .cloned()
        .collect())
}

pub fn decode(key: &str, message: &str) -> Option<String> {
    Some(message
        .chars()
        .zip(key.chars().cycle())
        .map(|(mc, kc)| decode_char(kc, mc))
        .collect::<Option<Vec<_>>>()?
        .iter()
        .cloned()
        .collect())
}

pub fn encode_random(s: &str) -> (String, String) {
    unimplemented!(
        "Generate random key with only a-z chars and encode {}. Return tuple (key, encoded s)",
        s
    )
}

const NUM_LETTERS: u8 = 26;

fn encode_char(key_char: char, message_char: char) -> Option<char> {
    let start_char = get_start_char(message_char)?;
    let message_index = message_char as u8 - start_char;
    let key_index = key_char as u8 - get_start_char(key_char)?;
    let new_index = (message_index + key_index) % NUM_LETTERS;
    let new_char = start_char + new_index;
    Some(new_char as char)
}

fn decode_char(key_char: char, message_char: char) -> Option<char> {
    let start_char = get_start_char(message_char)?;
    let message_index = message_char as u8 - start_char;
    let key_index = key_char as u8 - get_start_char(key_char)?;
    dbg!((message_index, NUM_LETTERS, key_index));
    let new_index = dbg!(message_index + NUM_LETTERS - key_index) % NUM_LETTERS;
    let new_char = start_char + new_index;
    Some(new_char as char)
}

fn get_start_char(c: char) -> Option<u8> {
    if c.is_ascii_lowercase() {
        Some(b'a')
    } else {
        None
    }
}
