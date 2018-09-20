use std::collections::HashSet

pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    let mut multiples = HashSet::new();
    for factor in factors.iter() {
        // Add all multiples of factor to `multiples`
    }
    multiples.sum()
}
