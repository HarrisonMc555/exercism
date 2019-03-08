pub fn abbreviate(phrase: &str) -> String {
    let words = get_words(phrase);
    words
        .iter()
        .map(|s| get_first_letter(&s).map(|c| c.to_ascii_uppercase()))
        .collect::<Option<_>>()
        .unwrap_or("".to_string())
}

fn get_words(phrase: &str) -> Vec<String> {
    phrase.split_whitespace().map(String::from).collect()
}

fn get_first_letter(word: &str) -> Option<char> {
    word.chars().next()
}
