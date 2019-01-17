#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::collections::HashSet;
use regex::Regex;
use permutohedron::Heap;

type Solution = HashMap<char, u8>;

pub fn solve(input: &str) -> Option<Solution> {
    let (lh1, lh2, rh) = parse_alphametic(input);
    let letters = extract_all_letters(&[&lh1, &lh2, &rh]);
    for possible_solution in get_possible_solutions(letters) {
        if is_valid_solution(&possible_solution, (&lh1, &lh2, &rh)) {
            return Some(possible_solution);
        }
    }
    None
}

fn parse_alphametic(input: &str) -> (String, String, String) {
    lazy_static! {
        static ref re: Regex =
            Regex::new(r"(\w+) *\+ *(\w+) *== *(\w+)").unwrap();
    }
    let cap = re.captures(input).expect("Improperly formed alphametic");
    (cap[0].to_string(), cap[1].to_string(), cap[2].to_string())
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

fn get_possible_solutions(letters: HashSet<char>) -> Vec<Solution> {
    Vec::new()
}

fn is_valid_solution(
    solution: &Solution,
    equation: (&str, &str, &str),
) -> bool {
    false
}

// struct SolutionGenerator<'a> {
//     letters: Vec<char>,
//     data: Vec<u8>,
//     heap: Option<Heap<'a, Vec<u8>, u8>>,
// }

// impl<'a> SolutionGenerator<'a> {
//     fn new(letters: Vec<char>, digits: &'a [u8]) -> Self {
//         let mut data = digits.to_vec();
//         let mut sg = SolutionGenerator {
//             letters,
//             data,
//             heap: None,
//         };
//         sg.heap = Some(Heap::new(&mut sg.data));
//         sg
//     }
// }

// struct PairwiseGenerator<T, U> {
//     firsts: Vec<T>,
//     seconds: Vec<U>,
// }

struct Permutations<T> {
    data: Vec<T>,
    length: usize,
    cycles: Vec<usize>,
    indices: Vec<usize>,
    index: usize,
}

impl<T> Permutations<T> {
    fn new(data: Vec<T>) -> Self {
        Permutations::with_length(data, data.len())
    }

    fn with_length(data: Vec<T>, length: usize) -> Self {
        let data_len = data.len();
        Permutations {
            data,
            length,
            cycles: (data_len - length + 1..=data_len).rev().collect(),
            indices: (0..data_len).collect(),
            index: length - 1,
        }
    }
}

impl<T> Iterator for Permutations<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.index;
        self.cycles[i] -= 1;
        if self.cycles[i] == 0 {
            self.indices = self.indices.splice(i.., vec![4,5]).collect();
            // self.indices = self.indices.splice(i.., self.indices[i+1..]).extend(self.indices[i]).collect();
            self.indices = self.indices.splice(i.., self.indices[i+1..]).collect();
        }
        None
    }
}
