pub fn nth(n: u32) -> u32 {
    let mut primes = vec![2];
    for _ in 0..n {
        let next_prime = next_prime(&primes);
        primes.push(next_prime);
    }
    primes[n as usize]
}

fn next_prime(primes: &[u32]) -> u32 {
    let mut x = primes[primes.len() - 1] + 1;
    loop {
        if primes.iter().all(|prime| x % prime != 0) {
            return x;
        }
        x += 1;
    }
}
