pub fn encrypt(input: &str) -> String {
    let input = normalize(input);
    unimplemented!("Encrypt {:?} using a square code", input)
}

fn normalize(input: &str) -> String {
    input
        .chars()
        .filter(|c| c.is_ascii_alphabetic())
        .map(|c| c.to_ascii_lowercase())
        .collect()
}
