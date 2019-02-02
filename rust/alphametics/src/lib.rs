#[macro_use]
extern crate lazy_static;

use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;

type Problem = (Vec<Vec<char>>, Vec<char>, usize);
type Solution = HashMap<char, u8>;
const BASE: u32 = 10;

pub fn solve(input: &str) -> Option<Solution> {
    let problem = parse_alphametic(input)?;
    find_solution(&problem)
}

lazy_static! {
    static ref SR1_RE: Regex = Regex::new(r"^ *(?P<sr1>\w+)").unwrap();
    static ref SRN_RE: Regex = Regex::new(r"\+ *(?P<srn>\w+)").unwrap();
    static ref RES_RE: Regex = Regex::new(r"== *(?P<res>\w+) *$").unwrap();
    static ref _RE: Regex =
        Regex::new(r"(?P<sr1>\w+)(?: *\+ *(?P<srn>\w+))* *== *(?P<res>\w+)")
            .unwrap();
}

fn parse_alphametic(input: &str) -> Option<Problem> {
    let sr1: Vec<_> = SR1_RE
        .captures(input)?
        .name("sr1")?
        .as_str()
        .chars()
        .rev()
        .collect();
    for caps in SRN_RE.captures_iter(input) {
        println!("{:?}", caps);
    }
    let srns = SRN_RE
        .captures_iter(input)
        .map(|caps| caps["srn"].chars().rev().collect::<Vec<_>>());
    let result: Vec<_> = RES_RE
        .captures(input)?
        .name("res")?
        .as_str()
        .chars()
        .rev()
        .collect();
    let sources: Vec<_> = Some(sr1).into_iter().chain(srns).collect();
    // transpose (want each element in 'sources' to be one column, not one row)
    let num_digits = max_digits(&sources[..]).unwrap_or(0);
    let sources_transposed = (0..num_digits)
        .map(|index| {
            sources
                .iter()
                .flat_map(|source| source.get(index))
                .cloned()
                .collect()
        })
        .collect();
    let num_digits = std::cmp::max(num_digits, result.len());
    println!(
        "{} -> {:?}",
        &input,
        Some((&sources_transposed, &result, num_digits))
    );
    Some((sources_transposed, result, num_digits))
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
    // println!(
    //     "rsh: solution({}), letters({}), numbers({})",
    //     solution.len(),
    //     remaining_letters.len(),
    //     remaining_numbers.len()
    // );
    // println!("rsh: {:?} ({:?})", solution, remaining_letters);
    // let solution_is_real = solution.get(&'L') == Some(&0)
    //     && solution.get(&'B') == Some(&9)
    //     && solution.get(&'I') == Some(&1);
    // if solution_is_real {
    //     println!("find_solution_helper");
    //     println!("\tsolution so far: {:?}", solution);
    //     println!("\tremaining letters: {:?}", remaining_letters);
    //     println!("\tremaining numbers: {:?}", remaining_numbers.iter());
    //     println!(
    //         "is_correct_solution: {:?}",
    //         is_correct_solution(problem, &solution, true)
    //     );
    // }
    match is_correct_solution(problem, &solution, false) {
        Some(true) => return true,
        Some(false) => return false,
        None => (),
    }
    // let real_solution = {
    //     let mut hash = HashMap::new();
    //     hash.insert('L', 0);
    //     hash.insert('B', 9);
    //     hash.insert('I', 1);
    //     hash
    // };
    assert!(!remaining_letters.is_empty());
    let letter = remaining_letters[0];
    let next_remaining_letters = &remaining_letters[1..];
    let remaining_numbers_cached = remaining_numbers.clone();
    for number in remaining_numbers_cached {
        solution.insert(letter, number);
        remaining_numbers.remove(&number);
        let found_solution = find_solution_helper(
            problem,
            solution,
            next_remaining_letters,
            &mut remaining_numbers,
        );
        if found_solution {
            return true;
        }
        solution.remove(&letter);
        remaining_numbers.insert(number);
    }
    false
}

fn is_correct_solution(
    problem: &Problem,
    solution: &Solution,
    _debug: bool,
) -> Option<bool> {
    let (sources, result, num_digits) = problem;
    // if debug {
    //     println!("num_digits: {}", num_digits)
    // };
    let mut carry: u32 = 0;
    // let
    for index in 0..*num_digits {
        // if debug {
        //     println!("\tindex = {}", index);
        //     println!("\tcarry = {}", carry);
        // };
        // if debug {
        //     println!("\tresult_char = ...")
        // };
        let result_char = match result.get(index) {
            Some(digit) => digit,
            None => return Some(false), // more source digits than result
        };
        // if debug {
        //     println!("\t\t{}", result_char)
        // };
        let result_digit = *solution.get(result_char)?;
        // if debug {
        //     println!("\tresult_digit: {}", result_digit)
        // };

        // if debug {
        //     println!("\tsource_chars = ...");
        // };
        let source_chars = match sources.get(index) {
            Some(chars) => chars,
            // If we're out of source digits, then the final result must be the
            // carry
            None => return Some(carry == u32::from(result_digit)),
        };
        // if debug {
        //     println!("\t\t{:?}", source_chars)
        // };
        let source_digits = source_chars
            .iter()
            .map(|c| solution.get(c))
            .collect::<Option<Vec<_>>>()?;
        // if debug {
        //     println!("\tsource_digits: {:?}", source_digits)
        // };

        let source_sum: u32 = source_digits
            .iter()
            .fold(0, |sum, &&digit| sum + u32::from(digit))
            + carry;
        // if debug {
        //     println!("\tsource_sum: {}", source_sum)
        // };
        let source_sum_digit = source_sum % BASE;
        // if debug {
        //     println!("\tsource_sum_digit: {}", source_sum_digit)
        // };
        if u32::from(result_digit) != source_sum_digit {
            // if debug {
            //     println!(
            //         "\tresult_digit != source_sum_digit\n\t{} != {}",
            //         result_digit, source_sum_digit
            //     );
            // };
            return Some(false);
        }
        carry = source_sum / BASE;
    }
    // There should be no carry when all is said and done
    Some(carry == 0)
}

fn extract_all_letters(problem: &Problem) -> Vec<char> {
    let (sources, result, num_digits) = problem;
    // let num_digits = std::cmp::max(sources.len(), result.len());
    let num_digits = *num_digits;
    let mut letters = Vec::with_capacity(num_digits);
    let mut cache = HashSet::with_capacity(num_digits);
    for index in 0..num_digits {
        for &c in sources
            .iter()
            .flat_map(|source| source.get(index))
            .chain(result.get(index))
        {
            if !cache.contains(&c) {
                letters.push(c);
                cache.insert(c);
            }
        }
    }
    letters
    // let all_letters = sources.iter().chain(Some(result)).fold(
    //     HashSet::new(),
    //     |mut all_letters, letters| {
    //         all_letters.extend(letters.iter());
    //         all_letters
    //     },
    // );
    // println!("all_letters: {:?}", all_letters);
    // all_letters.iter().cloned().collect()
}

fn get_all_digits(base: u8) -> HashSet<u8> {
    (0..base).fold(HashSet::new(), |mut s, n| {
        s.insert(n);
        s
    })
}

fn max_digits<T>(words: &[Vec<T>]) -> Option<usize> {
    words.iter().map(|word| word.len()).max()
}
