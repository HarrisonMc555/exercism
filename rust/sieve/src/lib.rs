pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    let upper_bound = upper_bound + 1;
    let mut sieve = vec![false; upper_bound as usize];
    let mut primes = Vec::new();
    let mut maybe_prime = next_prime(&sieve);
    while maybe_prime.is_some() {
        let prime = maybe_prime.unwrap();
        primes.push(prime);
        mark_multiples(&mut sieve, prime);
        maybe_prime = next_prime(&sieve);
    }
    primes
}

fn next_prime(sieve: &[bool]) -> Option<u64> {
    sieve
        .iter()
        .enumerate()
        .skip(2)
        .find(|&(_, b)| !b)
        .map(|(x, _)| x as u64)
}

fn mark_multiples(sieve: &mut [bool], prime: u64) {
    for i in (0..sieve.len()).step_by(prime as usize) {
        sieve[i] = true;
    }
}
