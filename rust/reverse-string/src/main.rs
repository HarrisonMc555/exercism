use reverse_string::reverse;

fn main() {
    for word in &["Hello", "Héllo", "中文", "uüu"] {
        println!(
            "Word: [{}], naive: [{}], correct: [{}]",
            word,
            word.chars().rev().collect::<String>(),
            reverse(word)
        );
    }
}
