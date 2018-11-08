const DIAGRAPH_PATTERNS: [&'static str; 4] = ["ch", "qu", "th", "thr"];
const DIAGRAPH_AFTER_VOWEL_PATTERNS: [&'static str; 1] = ["qu"];

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
    let first = get_first(word);
    let rest: String = word.chars().skip(first.chars().count()).collect();
    rest + &first + "ay"
}

fn get_first(word: &str) -> String {
    get_diagraph(word)
        .or_else(|| get_diagraph_after_vowel(word))
        .or_else(|| word.chars().next().map(|c| c.to_string()))
        .unwrap_or(String::from(word))
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

fn get_diagraph(word: &str) -> Option<String> {
    DIAGRAPH_PATTERNS
        .iter()
        .find(|&diagraph| word.starts_with(diagraph))
        .map(|&word| String::from(word))
}

fn get_diagraph_after_vowel(word: &str) -> Option<String> {
    let diagraph = DIAGRAPH_AFTER_VOWEL_PATTERNS.iter().find(|&diagraph| {
        word.chars()
            .skip(1)
            .collect::<String>()
            .starts_with(diagraph)
    });
    diagraph.map(|&diagraph| {
        word.chars().take(diagraph.chars().count() + 1).collect()
    })
}
