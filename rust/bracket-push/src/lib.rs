#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::collections::HashSet;

lazy_static! {
    static ref BRACKET_HASH: HashMap<char, char> = {
        let mut bracket_hash = HashMap::new();
        bracket_hash.insert('(', ')');
        bracket_hash.insert('[', ']');
        bracket_hash.insert('{', '}');
        bracket_hash
    };
    static ref CLOSE_BRACKETS: HashSet<char> = { BRACKET_HASH.values().cloned().collect() };
}

pub fn brackets_are_balanced(string: &str) -> bool {
    let mut stack = Vec::new();
    for c in string.chars() {
        if BRACKET_HASH.contains_key(&c) {
            stack.push(c);
        } else if CLOSE_BRACKETS.contains(&c) {
            let open = stack.pop();
            if open.is_none() || BRACKET_HASH[&open.unwrap()] != c {
                return false;
            }
        }
    }
    stack.is_empty()
}
