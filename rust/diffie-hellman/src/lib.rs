extern crate num_bigint;
extern crate num_traits;
extern crate primes;
extern crate rand;

use num_bigint::ToBigUint;
use num_traits::cast::ToPrimitive;
use primes::PrimeSet;
use rand::Rng;
use std::u64;

const NUM_PRIME_CHOICES: usize = 100;

pub fn private_key(p: u64) -> u64 {
    let valid_primes: Vec<_> = PrimeSet::new().iter().take_while(|x| *x < p).collect();
    let close_primes: Vec<_> = valid_primes
        .into_iter()
        .rev()
        .take(NUM_PRIME_CHOICES)
        .collect();
    let choice_ref = rand::thread_rng()
        .choose(&close_primes)
        .expect("Maximum range for private_key too small");
    *choice_ref
}

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    mod_exp(g, a, p)
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    mod_exp(b_pub, a, p)
}

fn mod_exp(x: u64, y: u64, n: u64) -> u64 {
    let x = ToBigUint::to_biguint(&x).unwrap();
    let y = ToBigUint::to_biguint(&y).unwrap();
    let n = ToBigUint::to_biguint(&n).unwrap();
    let result = x.modpow(&y, &n);
    // We know that result will be Ok because we took the modulus with a u64, so
    // the result must fit into a u64.
    result.to_u64().unwrap()
}

// // If we don't use BigUint
// fn mod_exp(x: u64, y: u64, n: u64) -> u64 {
//     if y == 0 { return 1 }
//     let z = mod_exp(x, y /2, n);
//     let result = if is_even(y) {
//         (z * z) % n
//     } else {
//         (x * z * z) % n
//     };
//     0
// }
