pub fn encode(source: &str) -> String {
    let mut encoded_portions = Vec::new();
    let source: Vec<_> = source.chars().collect();
    let mut slice = source.as_slice();
    while let Some((encoded, rest)) = encode_portion(slice) {
        encoded_portions.push(encoded);
        slice = rest;
    }
    encoded_portions.join("")
}

pub fn decode(source: &str) -> String {
    let mut decoded_portions = Vec::new();
    let source: Vec<_> = source.chars().collect();
    let mut slice = source.as_slice();
    while let Some((decoded, rest)) = decode_portion(slice) {
        decoded_portions.push(decoded);
        slice = rest;
    }
    decoded_portions.join("")
}

fn encode_portion(source: &[char]) -> Option<(String, &[char])> {
    if source.is_empty() {
        return None;
    }
    let first_char = source[0];
    let num_matching_chars = source.iter().take_while(|&&c| c ==
                                                      first_char).count();
    let rest = &source[num_matching_chars..];
    let encoded = if num_matching_chars == 1 {
        first_char.to_string()
    } else {
        format!("{}{}", num_matching_chars, first_char)
    };
    Some((encoded, rest))
}

fn decode_portion(source: &[char]) -> Option<(String, &[char])> {
    if source.is_empty() {
        return None;
    }
    let digit_chars: String = source
        .iter()
        .take_while(|&&c| c.is_ascii_digit())
        .collect();
    if digit_chars.is_empty() {
        let decoded = source[0].to_string();
        let rest = &source[1..];
        return Some((decoded, rest));
    }
    let num_digits = digit_chars.len();
    let letter = source[num_digits];
    let rest = &source[num_digits + 1..];
    let num_letters: usize = digit_chars.parse().unwrap();
    let decoded = std::iter::repeat(letter).take(num_letters as usize).collect();
    Some((decoded, rest))
}
