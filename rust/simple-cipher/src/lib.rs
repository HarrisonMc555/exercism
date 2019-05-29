use rand::distributions::{Distribution, Uniform};
use rand::{thread_rng, Rng};
use std::iter;

#[derive(Debug, Copy, Clone)]
enum CodingDirection {
    Encode,
    Decode,
}

pub fn encode(key: &str, message: &str) -> Option<String> {
    process(key, message, CodingDirection::Encode)
}

pub fn decode(key: &str, message: &str) -> Option<String> {
    process(key, message, CodingDirection::Decode)
}

pub fn encode_random(message: &str) -> (String, String) {
    let mut rng = thread_rng();
    let key = random_key(&mut rng);
    // This should never panic if we implement the random_key function
    // correctly
    let encoded = encode(&key, message).unwrap();
    (key, encoded)
}

fn process(
    key: &str,
    message: &str,
    direction: CodingDirection,
) -> Option<String> {
    if message.is_empty() || key.is_empty() {
        return None;
    }
    Some(
        message
            .chars()
            .zip(key.chars().cycle())
            .map(|(mc, kc)| process_char(kc, mc, direction))
            .collect::<Option<Vec<_>>>()?
            .iter()
            .cloned()
            .collect(),
    )
}

const NUM_LETTERS: u8 = 26;

fn process_char(
    key_char: char,
    message_char: char,
    direction: CodingDirection,
) -> Option<char> {
    let start_char = get_start_char(message_char)?;
    let message_index = message_char as u8 - start_char;
    let key_index = key_char as u8 - get_start_char(key_char)?;
    let new_index = match direction {
        CodingDirection::Encode => (message_index + key_index) % NUM_LETTERS,
        CodingDirection::Decode => {
            (message_index + NUM_LETTERS - key_index) % NUM_LETTERS
        }
    };
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

fn random_key<R>(rng: &mut R) -> String
where
    R: Rng,
{
    const MIN_KEY_LEN: usize = 100;
    const MAX_KEY_LEN: usize = 300;
    let key_len = rng.gen_range(MIN_KEY_LEN, MAX_KEY_LEN + 1);
    iter::repeat(())
        .map(|()| rng.sample(LowerCaseAlphabetic))
        .take(key_len)
        .collect()
}

#[derive(Debug)]
pub struct LowerCaseAlphabetic;

// Stolen from Alphanumeric and Standard
impl Distribution<char> for LowerCaseAlphabetic {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> char {
        const GEN_ASCII_STR_CHARSET: &[u8] = b"abcdefghijklmnopqrstuvwxyz";
        let range = Uniform::new(0, GEN_ASCII_STR_CHARSET.len());
        let index = range.sample(rng);
        GEN_ASCII_STR_CHARSET[index as usize] as char
    }
}
