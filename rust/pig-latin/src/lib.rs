pub fn translate(input: &str) -> String {
    if input.is_empty() {
        return String::from(input);
    }
    let first_letter = input.chars().next().unwrap();
    if is_vowel(first_letter) {
        input.to_owned() + "ay"
    } else {
        let rest: String = input.chars().skip(1).collect();
        rest + &first_letter.to_string() + "ay"
    }
}

fn is_vowel(letter: char) -> bool {
    let letter = letter.to_ascii_lowercase();
    match letter {
        'a' => true,
        'e' => true,
        'i' => true,
        'o' => true,
        'u' => true,
        _ => false,
    }
}
