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
    DIGIT_MAP
        .get(&LargeChar::try_parse(input)?)
        .map(Clone::clone)
}

lazy_static! {
    static ref ZERO: LargeChar = {
        LargeChar::try_parse(&vec![
            vec![' ', '_', ' '],
            vec!['|', ' ', '|'],
            vec!['|', '_', '|'],
            vec![' ', ' ', ' '],
        ])
        .unwrap()
    };
    static ref ONE: LargeChar = {
        LargeChar::try_parse(&vec![
            vec![' ', ' ', '|'],
            vec![' ', ' ', '|'],
            vec![' ', ' ', '|'],
            vec![' ', ' ', ' '],
        ])
        .unwrap()
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
    pub fn try_parse(lines: &DigitVec) -> Option<Self> {
        if !LargeChar::is_valid(lines) {
            return None;
        }
        let lc = LargeChar {
            lines: lines.to_vec(),
        };
        Some(lc)
    }

    const NUM_ROWS: usize = 4;
    const NUM_COLS: usize = 3;
    fn is_valid(lines: &DigitVec) -> bool {
        if lines.len() != LargeChar::NUM_ROWS {
            return false;
        }
        if !lines.iter().all(|row| row.len() == LargeChar::NUM_COLS) {
            return false;
        }
        true
    }
}
