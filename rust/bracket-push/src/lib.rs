#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;

lazy_static! {
    static ref BRACKET_HASH: HashMap<char, char> = {
        let mut bracket_hash = HashMap::new();
        bracket_hash.insert('(', ')');
        bracket_hash.insert('[', ']');
        bracket_hash.insert('{', '}');
        bracket_hash
    };
}

pub fn brackets_are_balanced(string: &str) -> bool {
    unimplemented!("Check if the string \"{}\" contains balanced brackets", string);
}
