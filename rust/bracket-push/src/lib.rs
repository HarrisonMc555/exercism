#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::collections::HashSet;

lazy_static! {
    static ref BRACKET_MATCH_HASH: HashMap<char, char> = {
        let mut bracket_hash = HashMap::new();
        bracket_hash.insert('(', ')');
        bracket_hash.insert('[', ']');
        bracket_hash.insert('{', '}');
        bracket_hash
    };
    static ref CLOSE_BRACKETS: HashSet<char> =
        { BRACKET_MATCH_HASH.values().cloned().collect() };
}

pub fn brackets_are_balanced(string: &str) -> bool {
    let mut stack = Vec::new();
    for c in string.chars() {
        if is_open_bracket(c) {
            // Opening brackets are pushed onto the stack
            stack.push(c);
        } else if is_close_bracket(c) && !close_bracket_matches(&mut stack, c) {
            // Closing brackets are checked to see if they match
            return false;
        }
        // Non-brackets are ignored
    }
    // All the brackets should have been paired, if any remain they were
    // unmatched
    stack.is_empty()
}

fn close_bracket_matches(stack: &mut Vec<char>, close: char) -> bool {
    let open = stack.pop();
    // True if the stack contained something AND it matches the closing bracket
    open.is_some() && BRACKET_MATCH_HASH[&open.unwrap()] == close
}

fn is_open_bracket(c: char) -> bool {
    BRACKET_MATCH_HASH.contains_key(&c)
}

fn is_close_bracket(c: char) -> bool {
    CLOSE_BRACKETS.contains(&c)
}
