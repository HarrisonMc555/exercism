use std::collections::{HashMap, HashSet};
use counter::Counter;

use regex::Regex;

const BASE: u32 = 10;

#[derive(Debug)]
struct Problem {
    columns: Vec<Column>,
    leading_letters: Vec<char>,
}

#[derive(Debug)]
struct Column {
    sum_letter: char,
    addend_letters: Counter<char>,
}

#[derive(Debug, Eq, PartialEq)]
enum SolutionStatus {
    ValidComplete,
    ValidIncomplete,
    Invalid,
}

type Solution = HashMap<char, u8>;

bitflags::bitflags! {
    #[rustfmt::skip]
    struct Digits: u32 {
        const ZERO =  0b0000000001;
        const ONE =   0b0000000010;
        const TWO =   0b0000000100;
        const THREE = 0b0000001000;
        const FOUR =  0b0000010000;
        const FIVE =  0b0000100000;
        const SIX =   0b0001000000;
        const SEVEN = 0b0010000000;
        const EIGHT = 0b0100000000;
        const NINE =  0b1000000000;
    }
}

const ALL_DIGITS: [Digits; 10] = [
    Digits::ZERO,
    Digits::ONE,
    Digits::TWO,
    Digits::THREE,
    Digits::FOUR,
    Digits::FIVE,
    Digits::SIX,
    Digits::SEVEN,
    Digits::EIGHT,
    Digits::NINE,
];

lazy_static::lazy_static! {
    static ref SR1_RE: Regex = Regex::new(r"^ *(?P<sr1>\w+)").unwrap();
    static ref SRN_RE: Regex = Regex::new(r"\+ *(?P<srn>\w+)").unwrap();
    static ref SUM_RE: Regex = Regex::new(r"== *(?P<sum>\w+) *$").unwrap();
    static ref _RE: Regex =
        Regex::new(r"(?P<sr1>\w+)(?: *\+ *(?P<srn>\w+))* *== *(?P<sum>\w+)")
            .unwrap();
}

pub fn solve(input: &str) -> Option<Solution> {
    // Get all letters
    let problem = parse_alphametic(input)?;
    let taken = Digits::empty();
    let mut solution = HashMap::new();
    let letters = get_ordered_letters(&problem);
    helper(&problem, taken, &mut solution, &letters[..]).map(|()| solution)
}

fn helper(
    problem: &Problem,
    taken: Digits,
    solution: &mut Solution,
    letters: &[char],
) -> Option<()> {
    match problem.evaluate(solution) {
        SolutionStatus::ValidComplete => return Some(()),
        SolutionStatus::ValidIncomplete => (),
        SolutionStatus::Invalid => return None,
    }
    let cur_letter = match letters.first() {
        Some(c) => c,
        None => {
            if problem.is_valid_solution(solution) {
                return Some(());
            } else {
                return None;
            }
        }
    };
    for digit in ALL_DIGITS.iter().filter(|d| !taken.contains(**d)) {
        solution.insert(*cur_letter, digit.to_u8());
        if helper(problem, taken | *digit, solution, &letters[1..]).is_some() {
            return Some(());
        }
    }
    solution.remove(cur_letter);
    None
}

fn get_ordered_letters(problem: &Problem) -> Vec<char> {
    let mut letters = Vec::new();
    let mut seen = HashSet::new();
    let mut process_letter = |c: char| {
        if !seen.contains(&c) {
            letters.push(c);
            seen.insert(c);
        }
    };
    for column in problem.columns.iter() {
        for (addend_letter, _) in column.addend_letters.iter() {
            process_letter(*addend_letter);
        }
        process_letter(column.sum_letter);
    }
    letters
}

impl Digits {
    pub fn to_u8(self) -> u8 {
        self.bits().trailing_zeros() as u8
    }
}

impl Problem {
    pub fn evaluate(&self, solution: &Solution) -> SolutionStatus {
        // No leading zeros
        if self
            .leading_letters
            .iter()
            .any(|c| solution.get(c) == Some(&0))
        {
            println!(
                "One of these leading digits:{:?} was zero, solution:{:?}",
                self.leading_letters, solution
            );
            return SolutionStatus::Invalid;
        }
        if solution
            == &IntoIterator::into_iter([
                ('S', 2),
                ('M', 0),
                ('O', 9),
                ('A', 8),
            ])
            .collect::<HashMap<_, _>>()
        {
            println!("Custom breakpoint");
        }
        let mut carry = 0;
        for column in self.columns.iter() {
            let sum_digit = match solution.get(&column.sum_letter) {
                Some(d) => *d,
                None => return SolutionStatus::ValidIncomplete,
            };
            let mut sum: u32 = carry;
            for (letter, count) in column.addend_letters.iter() {
                let digit = match solution.get(letter) {
                    Some(d) => d,
                    None => return SolutionStatus::ValidIncomplete,
                };
                sum += u32::from(*digit) * (*count as u32);
            }
            if sum % BASE != sum_digit as u32 {
                return SolutionStatus::Invalid;
            }
            carry = sum / BASE;
        }
        SolutionStatus::ValidComplete
    }

    pub fn is_valid_solution(&self, solution: &Solution) -> bool {
        self.evaluate(solution) == SolutionStatus::ValidComplete
    }
}

fn parse_alphametic(input: &str) -> Option<Problem> {
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
    let sum_letters: Vec<_> = SUM_RE
        .captures(input)?
        .name("sum")?
        .as_str()
        .chars()
        .rev()
        .collect();
    let addends: Vec<_> = std::iter::once(sr1).chain(srns).collect();
    let num_digits = max_len(&addends[..]).unwrap_or(0);
    // Transpose (want each element in 'sources' to be one column, not one row)
    let addend_letters_columns = (0..num_digits)
        .map(|index| {
            addends
                .iter()
                .flat_map(|source| source.get(index))
                .cloned()
                .collect()
        })
        .collect::<Vec<Counter<_>>>();
    if addend_letters_columns.len() > sum_letters.len() {
        return None;
    }
    println!("Addends:{:?}, sum_letters:{:?}", addends, sum_letters);
    println!(
        "Addends firsts:{:?}, sum_letters first:{:?}",
        addends.iter().flat_map(|a| a.last()).collect::<Vec<_>>(),
        sum_letters.last()
    );
    let leading_letters = addends
        .iter()
        .flat_map(|a| a.last())
        .chain(sum_letters.last())
        .copied()
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
    let addend_letters_columns_iter = addend_letters_columns
        .into_iter()
        .chain(std::iter::repeat_with(Counter::new));
    let columns = sum_letters
        .into_iter()
        .zip(addend_letters_columns_iter.into_iter())
        .map(|(sum_letter, addend_letters)| Column {
            sum_letter,
            addend_letters,
        })
        .collect();
    Some(Problem {
        columns,
        leading_letters,
    })
}

fn max_len<T>(words: &[Vec<T>]) -> Option<usize> {
    words.iter().map(|word| word.len()).max()
}

#[cfg(test)]
mod test {
    use crate::SolutionStatus::ValidIncomplete;

    use super::*;

    lazy_static::lazy_static! {
        static ref SOLUTION: Solution = to_hash_map([('A', 1), ('B', 2), ('C', 3)]);
        static ref INVALID_SOLUTION: Solution = to_hash_map([('A', 1), ('B', 2), ('C',4)]);
        static ref PARTIAL_SOLUTION: Solution = to_hash_map([('A', 1), ('B', 2)]);
        static ref PROBLEM: Problem = Problem {
            columns: vec![
                Column { sum_letter: 'C', addend_letters: to_counter("AB")},
                Column { sum_letter: 'B', addend_letters: to_counter("AA")},
                Column { sum_letter: 'A', addend_letters: to_counter("A")},
            ],
            leading_letters: vec![ 'A' ],
        };
    }

    #[test]
    fn test_valid_solution() {
        assert!(PROBLEM.is_valid_solution(&SOLUTION));
        assert_eq!(PROBLEM.evaluate(&SOLUTION), SolutionStatus::ValidComplete);
    }

    #[test]
    fn test_invalid_solution() {
        assert!(!PROBLEM.is_valid_solution(&INVALID_SOLUTION));
        assert_eq!(
            PROBLEM.evaluate(&INVALID_SOLUTION),
            SolutionStatus::Invalid
        );
    }

    #[test]
    fn test_partial_solution() {
        assert!(!PROBLEM.is_valid_solution(&PARTIAL_SOLUTION));
        assert_eq!(PROBLEM.evaluate(&PARTIAL_SOLUTION), ValidIncomplete);
    }

    #[test]
    fn test_real_problem() {
        let problem = Problem {
            columns: vec![
                Column {
                    sum_letter: 'L',
                    addend_letters: to_counter("IB"),
                },
                Column {
                    sum_letter: 'L',
                    addend_letters: to_counter("B"),
                },
                Column {
                    sum_letter: 'I',
                    addend_letters: Counter::new(),
                },
            ],
            leading_letters: vec!['I', 'B'],
        };

        let invalid_solution = to_hash_map([('I', 0), ('B', 1), ('L', 2)]);
        assert_eq!(
            problem.evaluate(&invalid_solution),
            SolutionStatus::Invalid
        );
        assert!(!problem.is_valid_solution(&invalid_solution));

        let valid_solution = to_hash_map([('I', 1), ('B', 9), ('L', 0)]);
        assert_eq!(
            problem.evaluate(&valid_solution),
            SolutionStatus::ValidComplete
        );
        assert!(problem.is_valid_solution(&valid_solution));
    }

    #[test]
    fn test_no_leading_zeros() {
        let problem = Problem {
            columns: vec![
                Column {
                    sum_letter: 'M',
                    addend_letters: to_counter("SA"),
                },
                Column {
                    sum_letter: 'O',
                    addend_letters: to_counter("A"),
                },
                Column {
                    sum_letter: 'M',
                    addend_letters: Counter::new(),
                },
            ],
            leading_letters: vec!['A', 'M'],
        };

        let invalid_solution =
            to_hash_map([('A', 8), ('S', 2), ('M', 0), ('O', 9)]);
        assert_eq!(
            problem.evaluate(&invalid_solution),
            SolutionStatus::Invalid
        );
        assert!(!problem.is_valid_solution(&invalid_solution));

        let valid_solution =
            to_hash_map([('A', 9), ('S', 2), ('M', 1), ('O', 0)]);
        assert_eq!(
            problem.evaluate(&valid_solution),
            SolutionStatus::ValidComplete
        );
        assert!(problem.is_valid_solution(&valid_solution));
    }

    fn to_hash_map<K: Eq + std::hash::Hash, V, const T: usize>(
        entries: [(K, V); T],
    ) -> HashMap<K, V> {
        IntoIterator::into_iter(entries).collect()
    }

    fn to_counter(string: &str) -> Counter<char> {
        string.chars().collect()
    }
}
