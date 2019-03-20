use std::collections::HashMap;

#[macro_use]
extern crate lazy_static;

type DigitVec = Vec<Vec<char>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidRowCount(usize),
    InvalidColumnCount(usize),
}

pub fn convert(input: &str) -> Result<String, Error> {
    unimplemented!("Convert the input '{}' to a string", input);
}

fn convert_digit(input: &DigitVec) -> Option<char> {
    DIGIT_MAP.get(&LargeChar::new(input)).map(Clone::clone)
}

lazy_static! {
    static ref ZERO: LargeChar = {
        LargeChar::new(&vec![
            vec![' ', '_', ' '],
            vec!['|', ' ', '|'],
            vec!['|', '_', '|'],
            vec![' ', ' ', ' '],
        ])
    };
    static ref ONE: LargeChar = {
        LargeChar::new(&vec![
            vec![' ', ' ', '|'],
            vec![' ', ' ', '|'],
            vec![' ', ' ', '|'],
            vec![' ', ' ', ' '],
        ])
    };
    static ref DIGIT_MAP: HashMap<LargeChar, char> = {
        let mut m = HashMap::new();
        m.insert(ZERO.clone(), '0');
        m.insert(ONE.clone(), '1');
        m
    };
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct LargeChar {
    lines: Vec<Vec<char>>,
}

impl LargeChar {
    pub fn new(lines: &[Vec<char>]) -> Self {
        LargeChar {
            lines: lines.to_vec(),
        }
    }
}
