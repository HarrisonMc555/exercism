extern crate primes;
extern crate rand;

use primes::PrimeSet;
use rand::Rng;

const NUM_PRIME_CHOICES: usize = 100;

pub fn private_key(p: u64) -> u64 {
    let valid_primes: Vec<_> = PrimeSet::new().iter()
        .take_while(|x| *x <= p).collect();
    let close_primes: Vec<_> = valid_primes.iter()
        .rev().take(NUM_PRIME_CHOICES).collect();
    let choice_ref_ref = rand::thread_rng().choose(&close_primes)
        .expect("Maximum range for private_key too small");
    **choice_ref_ref
}

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    unimplemented!(
        "Calculate public key using prime numbers {} and {}, and private key {}",
        p,
        g,
        a
    )
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    unimplemented!(
        "Calculate secret key using prime number {}, public key {}, and private key {}",
        p,
        b_pub,
        a
    )
}
