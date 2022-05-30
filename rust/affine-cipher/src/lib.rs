use itertools::Itertools;

/// While the problem description indicates a return status of 1 should be returned on errors,
/// it is much more common to return a `Result`, so we provide an error type for the result here.
#[derive(Debug, Eq, PartialEq)]
pub enum AffineCipherError {
    NotCoprime(i32),
}

const FIRST_LETTER: char = 'a';
const FIRST_LETTER_INDEX: i32 = FIRST_LETTER as i32;
const NUM_LETTERS: i32 = 26;

const CHUNK_SIZE: usize = 5;

/// Encodes the plaintext using the affine cipher with key (`a`, `b`). Note that, rather than
/// returning a return code, the more common convention in Rust is to return a `Result`.
pub fn encode(plaintext: &str, a: i32, b: i32) -> Result<String, AffineCipherError> {
    find_mmi(a, NUM_LETTERS)?;
    #[allow(unstable_name_collisions)]
    Ok(plaintext
        .chars()
        .filter(char::is_ascii_alphanumeric)
        .map(|c| encode_letter(c, a, b))
        .chunks(CHUNK_SIZE)
        .into_iter()
        .map(|chunk| chunk.into_iter().collect::<String>())
        .intersperse(" ".to_string())
        .collect::<String>())
}

/// Decodes the ciphertext using the affine cipher with key (`a`, `b`). Note that, rather than
/// returning a return code, the more common convention in Rust is to return a `Result`.
pub fn decode(ciphertext: &str, a: i32, b: i32) -> Result<String, AffineCipherError> {
    let a_mmi = find_mmi(a, NUM_LETTERS)?;
    Ok(ciphertext
        .chars()
        .filter(char::is_ascii_alphanumeric)
        .map(|c| decode_letter(c, a_mmi, b))
        .collect())
}

fn encode_letter(letter: char, a: i32, b: i32) -> char {
    transform_letter_by_index(letter, |x| (a * x + b).rem_euclid(NUM_LETTERS))
}

fn decode_letter(letter: char, a_mmi: i32, b: i32) -> char {
    transform_letter_by_index(letter, |y| (a_mmi * (y - b)).rem_euclid(NUM_LETTERS))
}

fn transform_letter_by_index<F>(letter: char, transform: F) -> char
where F: Fn(i32) -> i32{
    if !letter.is_ascii_alphabetic() {
        return letter;
    }
    let prev_index = get_letter_index(letter.to_ascii_lowercase());
    let next_index = transform(prev_index);
    from_letter_index(next_index)
}

fn get_letter_index(letter: char) -> i32 {
    letter as i32 - FIRST_LETTER_INDEX
}

fn from_letter_index(index: i32) -> char {
    char::from_u32((index + FIRST_LETTER_INDEX) as u32)
        .unwrap_or_else(|| panic!("Invalid letter index: {}", index))
}

fn find_mmi(a: i32, n: i32) -> Result<i32, AffineCipherError> {
    let mut t = 0;
    let mut new_t = 1;
    let mut r = n;
    let mut new_r = a;

    while new_r != 0 {
        let quotient = r / new_r;
        (t, new_t) = (new_t, t - quotient * new_t);
        (r, new_r) = (new_r, r - quotient * new_r);
    }

    if r > 1 {
        return Err(AffineCipherError::NotCoprime(a));
    }

    if t < 0 {
        t += n;
    }
    Ok(t)
}

#[cfg(test)]
mod test {
    use crate::{find_mmi, AffineCipherError};

    #[test]
    fn test_mmi_simple() {
        assert_eq!(find_mmi(9, 26), Ok(3));
    }

    #[test]
    fn test_mmi_complicated() {
        assert_eq!(find_mmi(15, 26), Ok(7));
    }

    #[test]
    fn test_mmi_not_coprime() {
        assert_eq!(find_mmi(13, 26), Err(AffineCipherError::NotCoprime(13)));
    }
}
