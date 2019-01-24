const DIAGRAPHS: [&str; 6] = ["ch", "qu", "thr", "th", "sch", "rh"];
const DIAGRAPHS_AFTER_VOWEL: [&str; 1] = ["qu"];
const VOWEL_DIAGRAPHS: [&str; 1] = ["xr"];

pub fn translate(input: &str) -> String {
    input
        .split(' ')
        .map(|word| translate_word(word))
        .collect::<Vec<_>>()
        .join(" ")
}

fn translate_word(word: &str) -> String {
    if word.is_empty() {
        return String::from(word);
    }
    if starts_with_vowel(word) {
        translate_word_starting_with_vowel(word)
    } else {
        translate_word_starting_with_consonant(word)
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
        .unwrap_or_else(|| String::from(word))
}

fn get_diagraph(word: &str) -> Option<String> {
    DIAGRAPHS
        .iter()
        .find(|&diagraph| word.starts_with(diagraph))
        .map(|&word| String::from(word))
}

fn get_diagraph_after_vowel(word: &str) -> Option<String> {
    let diagraph = DIAGRAPHS_AFTER_VOWEL.iter().find(|&diagraph| {
        word.chars()
            .skip(1)
            .collect::<String>()
            .starts_with(diagraph)
    });
    diagraph.map(|&diagraph| {
        word.chars().take(diagraph.chars().count() + 1).collect()
    })
}

fn starts_with_vowel(word: &str) -> bool {
    if word.is_empty() {
        return false;
    }
    let first_letter = word.chars().next().unwrap();
    // Regular vowels are vowels
    if is_regular_vowel(first_letter) {
        return true;
    }
    // Vowel diagraphs count as vowels
    if VOWEL_DIAGRAPHS
        .iter()
        .any(|&diagraph| word.starts_with(diagraph))
    {
        return true;
    }
    // Regular consonants are NOT vowels
    if first_letter != 'y' {
        return false;
    }
    // 'y' is only a vowel if followed by a consonant
    let second_letter = word.chars().nth(1);
    match second_letter {
        Some(c) => is_regular_consonant(c),
        None => true,
    }
}

fn is_regular_vowel(letter: char) -> bool {
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

fn is_regular_consonant(letter: char) -> bool {
    !is_regular_vowel(letter)
}
