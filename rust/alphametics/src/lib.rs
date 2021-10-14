use std::collections::HashMap;

use lazy_static;
use regex::Regex;
use bitflags;

#[derive(Debug)]
struct Problem {
    addend_digit_columns: Vec<Vec<char>>,
    sum_digits: Vec<char>,
    num_digits: usize,
}

bitflags::bitflags! {
    struct Digit: u32 {
        const NONE = 0b0000000000;
        const ZERO = 0b0000000001;
        const ONE = 0b0000000010;
        const TWO = 0b0000000100;
        const THREE = 0b0000001000;
        const FOUR = 0b0000010000;
        const FIVE = 0b0000100000;
        const SIX = 0b0001000000;
        const SEVEN = 0b0010000000;
        const EIGHT = 0b0100000000;
        const NINE = 0b1000000000;
    }
}

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    // Get all letters
    let problem = parse_alphametic(input)?;
    println!("{:?}", problem);
    let taken = Digit::NONE;
    unimplemented!()
}

lazy_static::lazy_static! {
    static ref SR1_RE: Regex = Regex::new(r"^ *(?P<sr1>\w+)").unwrap();
    static ref SRN_RE: Regex = Regex::new(r"\+ *(?P<srn>\w+)").unwrap();
    static ref SUM_RE: Regex = Regex::new(r"== *(?P<sum>\w+) *$").unwrap();
    static ref _RE: Regex =
        Regex::new(r"(?P<sr1>\w+)(?: *\+ *(?P<srn>\w+))* *== *(?P<sum>\w+)")
            .unwrap();
}

fn parse_alphametic(input: &str) -> Option<Problem> {
    // for caps in SRN_RE.captures_iter(input) {
    //     println!("{:?}", caps);
    // }
    let sr1: Vec<_> = SR1_RE
        .captures(input)?
        .name("sr1")?
        .as_str()
        .chars()
        .rev()
        .collect();
    let srns = SRN_RE
        .captures_iter(input)
        .map(|caps| caps["srn"].chars().rev().collect::<Vec<_>>());
    let sum_digits: Vec<_> = SUM_RE
        .captures(input)?
        .name("sum")?
        .as_str()
        .chars()
        .rev()
        .collect();
    let addends: Vec<_> = std::iter::once(sr1).chain(srns).collect();
    let num_digits = max_len(&addends[..]).unwrap_or(0);
    // transpose (want each element in 'sources' to be one column, not one row)
    let addend_digit_columns = (0..num_digits)
        .map(|index| {
            addends
                .iter()
                .flat_map(|source| source.get(index))
                .cloned()
                .collect()
        })
        .collect();
    let num_digits = std::cmp::max(num_digits, sum_digits.len());
    // println!(
    //     "{} -> {:?}",
    //     &input,
    //     Some((&sources_transposed, &result, num_digits))
    // );
    Some(Problem {
        addend_digit_columns,
        sum_digits,
        num_digits,
    })
}

fn max_len<T>(words: &[Vec<T>]) -> Option<usize> {
    words.iter().map(|word| word.len()).max()
}
