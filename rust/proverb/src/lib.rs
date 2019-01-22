const PROVERB_PREFIX: &str = "For want of a";
const PROVERB_MIDDLE: &str = "the";
const PROVERB_SUFFIX: &str = "was lost";
const PROVERB_LAST_LINE_PREFIX: &str = "And all for the want of a";

pub fn build_proverb(list: &[&str]) -> String {
    if list.is_empty() {
        return String::new();
    }
    let item_pairs = list.iter().zip(list[1..].iter());
    let mut lines: Vec<_> = item_pairs
        .map(|(item1, item2)| normal_line(item1, item2))
        .collect();
    let first_item = list[0];
    lines.push(last_line(first_item));
    lines.join("\n")
}

fn normal_line(item1: &str, item2: &str) -> String {
    make_sentence(&[
        PROVERB_PREFIX,
        item1,
        PROVERB_MIDDLE,
        item2,
        PROVERB_SUFFIX,
    ])
}

fn last_line(item: &str) -> String {
    make_sentence(&[PROVERB_LAST_LINE_PREFIX, item])
}

fn make_sentence(words: &[&str]) -> String {
    let mut str = words.join(" ");
    str.push('.');
    str
}
