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
    let lines: DigitVec =
        input.lines().map(|line| line.chars().collect()).collect();
    let rem = lines.len() % LargeChar::NUM_ROWS;
    if rem != 0 {
        return Err(Error::InvalidRowCount(rem));
    }
    let strings = lines
        .chunks(LargeChar::NUM_ROWS)
        // .map(|rows| convert_line(&rows.to_vec()));
        .map(convert_line);

    unimplemented!("Convert the input '{}' to a string", input);
}

fn convert_line(rows: &DigitVec) -> Result<String, Error> {
    let num_cols = rows[0].len();
    if let Some(invalid_length) =
        rows.iter().map(Vec::len).find(|len| *len != num_cols)
    {
        return Err(Error::InvalidColumnCount(invalid_length));
    }
    Ok("".to_string())
}

fn convert_digit(rows: &DigitVec) -> Result<char, Error> {
    Ok(DIGIT_MAP
        .get(&LargeChar::try_parse(rows)?)
        .map(Clone::clone)
        .unwrap_or('?'))
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
    rows: Vec<Vec<char>>,
}

impl LargeChar {
    const NUM_ROWS: usize = 4;
    const NUM_COLS: usize = 3;

    pub fn try_parse(rows: &DigitVec) -> Result<Self, Error> {
        if rows.len() != LargeChar::NUM_ROWS {
            return Err(Error::InvalidRowCount(rows.len()));
        }
        if let Some(col_count) = rows
            .iter()
            .map(Vec::len)
            .find(|row_len| *row_len != LargeChar::NUM_COLS)
        {
            return Err(Error::InvalidColumnCount(col_count));
        }
        let lc = LargeChar {
            rows: rows.to_vec(),
        };
        Ok(lc)
    }
}
