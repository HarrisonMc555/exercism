use std::collections::BTreeMap;

pub fn transform(
    score_to_letters: &BTreeMap<i32, Vec<char>>,
) -> BTreeMap<char, i32> {
    let mut result = BTreeMap::new();
    for (&score, letters) in score_to_letters {
        let lower_case_letters =
            letters.iter().map(char::to_ascii_lowercase);
        for letter in lower_case_letters {
            result.insert(letter, score);
        }
    }
    result
}
