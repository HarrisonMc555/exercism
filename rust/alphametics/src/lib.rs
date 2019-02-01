#[macro_use]
extern crate lazy_static;

use boolinator::Boolinator;
use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;

type Problem<'a> = (Vec<&'a str>, &'a str);
type Solution = HashMap<char, u8>;
const BASE: u32 = 10;

pub fn solve(input: &str) -> Option<Solution> {
    let (lh1, lh2, rh) = parse_alphametic(input);
    let problem: Problem = (&lh1, &lh2, &rh);
    find_solution(&problem)
}

lazy_static! {
    static ref RE: Regex =
        Regex::new(r"(?P<sr1>\w+)(?: *\+ *(?P<srn>\w+))* *== *(?P<res>\w+)")
            .unwrap();
}

fn parse_alphametic<'a>(input: &'a str) -> Option<Problem<'a>> {
    let cap = RE.captures(input)?;
    let mut sources = Vec::new();
    sources.push(cap.name("sr1")?.as_str());
    // Need to get multiple...
    if let Some(srn) = cap.name("srn") {
        sources.push(srn.as_str());
    }
    let result = cap.name("res")?.as_str();
    Some((sources, result))
}

fn find_solution(problem: &Problem) -> Option<Solution> {
    let (sources, result) = problem;
    let mut solution = HashMap::new();
    let all_words = sources.into_iter().chain(Some(result)).collect();
    let mut remaining_letters = extract_all_letters(all_words);
    let mut remaining_numbers = (0..BASE).fold(HashSet::new(), |mut s, n| {
        s.insert(n);
        s
    });
    find_solution_helper(
        problem,
        &mut solution,
        &mut remaining_letters,
        &mut remaining_numbers,
    )
    .map(|&solution| solution)
}

fn find_solution_helper<'a>(
    problem: &Problem,
    solution: &'a mut Solution,
    remaining_letters: &mut HashSet<char>,
    remaining_numbers: &mut HashSet<u32>,
) -> Option<&'a Solution> {
    if remaining_letters.is_empty() {
        return is_correct_solution(problem, &solution).as_some(solution);
    }
    None
}

fn is_correct_solution(problem: &Problem, solution: &Solution) -> bool {
    false
}

fn extract_all_letters(strings: &[&str]) -> HashSet<char> {
    let mut result = HashSet::new();
    for string in strings.iter() {
        for c in string.chars() {
            result.insert(c);
        }
    }
    result
}
