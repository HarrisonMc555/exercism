extern crate unicode_segmentation;

use unicode_segmentation::UnicodeSegmentation;

pub fn reverse(input: &str) -> String {
    let mut output = Vec::new();
    for c in UnicodeSegmentation::graphemes(input, true).rev() {
        output.push(c);
    }
    output.into_iter().collect()
}
