extern crate integer_sqrt;
extern crate slow_primes;

use integer_sqrt::IntegerSquareRoot;
use slow_primes::Primes;

pub fn factors(n: u64) -> Vec<u64> {
    let max_factor = n.integer_sqrt();
    let sieve = Primes::sieve(max_factor as usize);
    let mut prime_factors = Vec::new();
    let mut num = n;
    for prime in sieve.primes() {
        let prime = prime as u64;
        while num % prime == 0 {
            num /= prime;
            prime_factors.push(prime);
        }
        if num == 1 {
            break;
        }
    }
    if num != 1 {
        prime_factors.push(num);
    }
    prime_factors
}
