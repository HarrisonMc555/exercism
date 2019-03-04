pub fn verse(n: i32) -> String {
    let these_bottles = bottles_of_beer(n);
    let first_line =
        format!("{} on the wall, {}.", these_bottles, these_bottles);
    // let next_num = if n == 0 { 99 } else { n - 1 };
    let next_num = natural_mod(n - 1, 100);
    let next_bottles = bottles_of_beer(next_num);
    let action = if n == 0 {
        "go to the store and buy some more".to_string()
    } else {
        format!("take {} down and pass it around", pluralize("one", n))
    };
    let second_line = format!("{}, {} on the wall.", action, next_bottles);
    let first_line = capitalize_first_letter(&first_line);
    let second_line = capitalize_first_letter(&second_line);
    format!("{}\n{}\n", first_line, second_line)
}

pub fn sing(start: i32, end: i32) -> String {
    (end..=start).rev().map(|n| verse(n)).collect::<Vec<_>>().join("\n")
}

fn bottles_of_beer(n: i32) -> String {
    let num_bottles = num_or_no_more(n);
    let bottle = pluralize("bottle", n);
    format!("{} {} of beer", num_bottles, bottle)
}

fn pluralize(word: &str, n: i32) -> String {
    match (word, n) {
        ("one", 1) => "it".to_string(),
        ("one", _) => "one".to_string(),
        (_, 1) => word.to_string(),
        (_, _) => format!("{}s", word),
    }
}

fn num_or_no_more(n: i32) -> String {
    if n == 0 {
        "no more".to_string()
    } else {
        n.to_string()
    }
}

fn capitalize_first_letter(s: &str) -> String {
    let mut chars = s.chars();
    let first_letter = chars.next();
    let uppercased = first_letter.map(|c| c.to_uppercase().to_string());
    let rest: String = chars.collect();
    format!("{}{}", uppercased.unwrap_or_else(|| "".to_string()), rest)
}

fn natural_mod(num: i32, divisor: i32) -> i32 {
    ((num % divisor) + divisor) % divisor
}
