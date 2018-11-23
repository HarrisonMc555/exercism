#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::collections::HashSet;

lazy_static! {
    static ref VALID_NUCLEOTIDES: HashSet<char> = {
        let mut set = HashSet::new();
        set.insert('A');
        set.insert('T');
        set.insert('G');
        set.insert('C');
        set
    };
}

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    if !VALID_NUCLEOTIDES.contains(&nucleotide) {
        return Err(nucleotide);
    }
    dna.chars().fold(Ok(0), |num, c| {
        if c == nucleotide {
            num.map(|n| n + 1)
        } else if !VALID_NUCLEOTIDES.contains(&c) {
            Err(c)
        } else {
            num
        }
    })
}

pub fn nucleotide_counts(dna: &str) -> Result<HashMap<char, usize>, char> {
    VALID_NUCLEOTIDES
        .iter()
        .map(|&nucleotide| count(nucleotide, dna).map(|n| (nucleotide, n)))
        .collect()
}
