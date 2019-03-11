pub fn abbreviate(phrase: &str) -> String {
    let words = phrase.split(|c: char| !c.is_ascii_alphanumeric());
    let capitalized_first_letters =
        words.filter_map(get_capitalized_first_letters);
    let capitalized_strings =
        capitalized_first_letters.map(|v| v.iter().collect::<String>());
    capitalized_strings.collect::<Vec<_>>().join("")
}

fn get_capitalized_first_letters(word: &str) -> Option<Vec<char>> {
    let all_uppercase = word.chars().all(|c| c.is_ascii_uppercase());
    let none_uppercase = word.chars().all(|c| !c.is_ascii_uppercase());
    let mut first_letters = if all_uppercase || none_uppercase {
        get_first_letter(&word).map(|c| vec![c])?
    } else {
        // If there are some uppercase and some not, grab all the uppercase ones
        word.chars().filter(|c| c.is_ascii_uppercase()).collect()
    };
    for c in first_letters.iter_mut() {
        *c = c.to_ascii_uppercase();
    }
    Some(first_letters)
}

fn get_first_letter(word: &str) -> Option<char> {
    word.chars().next()
}
