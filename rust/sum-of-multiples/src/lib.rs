use std::collections::HashSet;

pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    let mut multiples = HashSet::new();
    for &factor in factors.iter() {
        for multiple in get_multiples(limit, factor) {
            multiples.insert(multiple);
        }
    }
    multiples.iter().sum()
}

fn get_multiples(limit: u32, factor: u32) -> std::iter::StepBy<std::ops::Range<u32>> {
    (factor..limit).step_by(factor as usize)
}
