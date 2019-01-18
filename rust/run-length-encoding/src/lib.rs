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
    unimplemented!("Return the run-length decoding of {}.", source);
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
