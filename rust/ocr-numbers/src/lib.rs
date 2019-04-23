use std::collections::HashMap;

#[macro_use]
extern crate lazy_static;

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidRowCount(usize),
    InvalidColumnCount(usize),
}

pub fn convert(input: &str) -> Result<String, Error> {
    let lines: Vec<Vec<char>> =
        input.lines().map(|line| line.chars().collect()).collect();
    let rem = lines.len() % LargeChar::NUM_ROWS;
    if rem != 0 {
        return Err(Error::InvalidRowCount(rem));
    }
    let strings: Result<Vec<String>, Error> = lines
        .chunks(LargeChar::NUM_ROWS)
        .map(convert_line)
        .collect();
    strings.map(|lines| lines.join(","))
}

fn convert_line(rows: &[Vec<char>]) -> Result<String, Error> {
    let num_cols = rows[0].len();
    if let Some(invalid_length) =
        rows.iter().map(Vec::len).find(|len| *len != num_cols)
    {
        return Err(Error::InvalidColumnCount(invalid_length));
    }
    let rem = num_cols % LargeChar::NUM_COLS;
    if rem != 0 {
        return Err(Error::InvalidColumnCount(num_cols));
    }
    let num_digits = num_cols / LargeChar::NUM_COLS;
    let vec_of_digit_lines = (0..num_digits).map(|i| {
        let first_index = i * LargeChar::NUM_COLS;
        let last_index = first_index + LargeChar::NUM_COLS;
        rows.iter()
            .map(|row| row[first_index..last_index].to_vec())
            .collect::<Vec<_>>()
    });
    let chars = vec_of_digit_lines
        .map(|x| convert_digit(&x.to_vec()))
        .collect::<Result<Vec<_>, Error>>()?;
    Ok(chars.iter().collect())
}

fn convert_digit(rows: &[Vec<char>]) -> Result<char, Error> {
    Ok(DIGIT_MAP
        .get(&LargeChar::try_parse(rows)?)
        .map(Clone::clone)
        .unwrap_or('?'))
}

fn strs_to_large_char(strs: &[&str]) -> LargeChar {
    let rows: Vec<Vec<char>> =
        strs.iter().map(|s| s.chars().collect()).collect();
    LargeChar::try_parse(&rows).unwrap()
}

lazy_static! {
    static ref ZERO: LargeChar =
        strs_to_large_char(&[" _ ", "| |", "|_|", "   "],);
    static ref ONE: LargeChar =
        strs_to_large_char(&["   ", "  |", "  |", "   "],);
    static ref TWO: LargeChar =
        strs_to_large_char(&[" _ ", " _|", "|_ ", "   "],);
    static ref THREE: LargeChar =
        strs_to_large_char(&[" _ ", " _|", " _|", "   "],);
    static ref FOUR: LargeChar =
        strs_to_large_char(&["   ", "|_|", "  |", "   "],);
    static ref FIVE: LargeChar =
        strs_to_large_char(&[" _ ", "|_ ", " _|", "   "],);
    static ref SIX: LargeChar =
        strs_to_large_char(&[" _ ", "|_ ", "|_|", "   "],);
    static ref SEVEN: LargeChar =
        strs_to_large_char(&[" _ ", "  |", "  |", "   "],);
    static ref EIGHT: LargeChar =
        strs_to_large_char(&[" _ ", "|_|", "|_|", "   "],);
    static ref NINE: LargeChar =
        strs_to_large_char(&[" _ ", "|_|", " _|", "   "],);
    static ref DIGIT_MAP: HashMap<LargeChar, char> = {
        let mut m = HashMap::new();
        m.insert(ZERO.clone(), '0');
        m.insert(ONE.clone(), '1');
        m.insert(TWO.clone(), '2');
        m.insert(THREE.clone(), '3');
        m.insert(FOUR.clone(), '4');
        m.insert(FIVE.clone(), '5');
        m.insert(SIX.clone(), '6');
        m.insert(SEVEN.clone(), '7');
        m.insert(EIGHT.clone(), '8');
        m.insert(NINE.clone(), '9');
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

    pub fn try_parse(rows: &[Vec<char>]) -> Result<Self, Error> {
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
