const ALPHABET_INITIAL_CHAR_UPPERCASE: char = 'A';
const ALPHABET_INITIAL_CHAR_UPPERCASE_INDEX: usize =
    ALPHABET_INITIAL_CHAR_UPPERCASE as usize;
const MAX_ALPHABET_INDEX: usize = 26;

pub fn get_diamond(c: char) -> Vec<String> {
    let num_chars = to_alphabet_index(c) + 1;
    (0..num_chars)
        .chain((0..num_chars).rev().skip(1))
        .map(|index| create_line(from_alphabet_index(index), index, num_chars))
        .collect()
}

fn create_line(c: char, index: usize, num_chars: usize) -> String {
    let num_trailing_spaces = index;
    let num_leading_spaces = num_chars - num_trailing_spaces - 1;
    let forward_string = format!(
        "{leading}{c}{trailing}",
        leading = " ".repeat(num_leading_spaces),
        trailing = " ".repeat(num_trailing_spaces),
        c = c,
    );
    let backward_string: String =
        forward_string.chars().rev().skip(1).collect();
    forward_string + &backward_string
}

fn to_alphabet_index(c: char) -> usize {
    let c = c.to_ascii_uppercase();
    assert!(c.is_ascii_uppercase());
    c as usize - ALPHABET_INITIAL_CHAR_UPPERCASE as usize
}

fn from_alphabet_index(index: usize) -> char {
    assert!(index <= MAX_ALPHABET_INDEX);
    (index + ALPHABET_INITIAL_CHAR_UPPERCASE_INDEX) as u8 as char
}
