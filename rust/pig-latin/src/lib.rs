const DIAGRAPH_PATTERNS: [&'static str; 4] = [
    // String::from("ch"),
    // String::from("qu"),
    // String::from("th"),
    // String::from("thr"),
    "ch",
    "qu",
    "th",
    "thr",
];

pub fn translate(input: &str) -> String {
    if input.is_empty() {
        return String::from(input);
    }
    let first_letter = input.chars().next().unwrap();
    if is_vowel(first_letter) {
        translate_word_starting_with_vowel(input)
    } else {
        translate_word_starting_with_consonant(input)
    }
}

fn translate_word_starting_with_vowel(word: &str) -> String {
    word.to_owned() + "ay"
}

fn translate_word_starting_with_consonant(word: &str) -> String {
    let first = get_diagraph(word);
    let rest: String = word.chars().skip(first.chars().count()).collect();
    rest + &first + "ay"
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

fn get_diagraph(word: &str) -> String {
    if word.is_empty() {
        return String::from(word);
    }
    DIAGRAPH_PATTERNS
        .into_iter()
        .find(|&diagraph| word.starts_with(diagraph))
        .map(|&word| String::from(word))
        .unwrap_or(word.chars().next().unwrap().to_string())
}
