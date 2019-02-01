#[macro_use]
extern crate lazy_static;

use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;

type Problem = (Vec<Vec<char>>, Vec<char>);
type Solution = HashMap<char, u8>;
const BASE: u32 = 10;

pub fn solve(input: &str) -> Option<Solution> {
    let problem = parse_alphametic(input)?;
    find_solution(&problem)
}

lazy_static! {
    static ref RE: Regex =
        Regex::new(r"(?P<sr1>\w+)(?: *\+ *(?P<srn>\w+))* *== *(?P<res>\w+)")
            .unwrap();
}

fn parse_alphametic(input: &str) -> Option<Problem> {
    let cap = RE.captures(input)?;
    let mut sources: Vec<Vec<char>> = Vec::new();
    sources.push(cap.name("sr1")?.as_str().chars().rev().collect());
    // Need to get multiple...
    if let Some(srn) = cap.name("srn") {
        sources.push(srn.as_str().chars().rev().collect());
    }
    // transpose (want each element in 'sources' to be one column, not one row)
    let num_digits: usize =
        sources.iter().map(|source| source.len()).max().unwrap_or(0);
    let sources_transposed = (0..num_digits)
        .map(|index| {
            sources
                .iter()
                .flat_map(|source| source.get(index))
                .cloned()
                .collect()
        })
        .collect();
    let result = cap.name("res")?.as_str().chars().rev().collect();
    Some((sources_transposed, result))
}

fn find_solution(problem: &Problem) -> Option<Solution> {
    let mut solution = HashMap::new();
    let remaining_letters = extract_all_letters(problem);
    let mut remaining_numbers = get_all_digits(BASE as u8);
    let found_solution = find_solution_helper(
        &problem,
        &mut solution,
        &remaining_letters,
        &mut remaining_numbers,
    );
    if found_solution {
        Some(solution)
    } else {
        None
    }
}

fn find_solution_helper<'a>(
    problem: &Problem,
    solution: &'a mut Solution,
    remaining_letters: &[char],
    mut remaining_numbers: &mut HashSet<u8>,
) -> bool {
    match is_correct_solution(problem, &solution) {
        Some(true) => return true,
        Some(false) => return false,
        None => (),
    }
    assert!(!remaining_letters.is_empty());
    let letter = remaining_letters[0];
    let next_remaining_letters = &remaining_letters[1..];
    let remaining_numbers_cached = remaining_numbers.clone();
    for number in remaining_numbers_cached {
        solution.insert(letter, number);
        remaining_numbers.remove(&number);
        if find_solution_helper(
            problem,
            solution,
            next_remaining_letters,
            &mut remaining_numbers,
        ) {
            return true;
        }
        solution.remove(&letter);
    }
    false
}

fn is_correct_solution(problem: &Problem, solution: &Solution) -> Option<bool> {
    let (sources, result) = problem;
    let num_digits = std::cmp::max(sources.len(), result.len());
    let mut carry = 0;
    // let
    for index in 0..num_digits {
        let source_chars = sources.get(index)?;
        let result_char = match result.get(index) {
            Some(digit) => digit,
            None => return Some(false), // more source digits than result
        };
        let source_digits = source_chars
            .iter()
            .map(|c| solution.get(c))
            .collect::<Option<Vec<_>>>()?;
        let result_digit = *solution.get(result_char)?;
        let source_sum: u32 =
            source_digits.iter().fold(0, |sum, &&digit| sum + u32::from(digit))
            + carry;
        let source_sum_digit = source_sum / BASE;
        if u32::from(result_digit) != source_sum_digit {
            return Some(false);
        }
        carry = source_sum % BASE;
    }
    // There should be no carry when all is said and done
    Some(carry == 0)
}

fn extract_all_letters(problem: &Problem) -> Vec<char> {
    let (sources, result) = problem;
    let all_letters = sources.iter().chain(Some(result)).fold(
        HashSet::new(),
        |mut all_letters, letters| {
            all_letters.extend(letters.iter());
            all_letters
        },
    );
    all_letters.iter().cloned().collect()
}

fn get_all_digits(base: u8) -> HashSet<u8> {
    (0..base).fold(HashSet::new(), |mut s, n| {
        s.insert(n);
        s
    })
}
