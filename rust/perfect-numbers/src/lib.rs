use integer_sqrt::IntegerSquareRoot;
use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq)]
pub enum Classification {
    Abundant,
    Perfect,
    Deficient,
}

pub fn classify(num: u64) -> Option<Classification> {
    if num == 0 {
        return None;
    }
    let aliquot = aliquot_sum(num);
    let ordering = match aliquot.cmp(&num) {
        Ordering::Equal => Classification::Perfect,
        Ordering::Greater => Classification::Abundant,
        Ordering::Less => Classification::Deficient,
    };
    Some(ordering)
}

fn aliquot_sum(num: u64) -> u64 {
    println!("Factors for {} are {:?}", num, factors(num));
    factors(num).iter().sum()
}

fn factors(num: u64) -> Vec<u64> {
    if num <= 1 {
        return Vec::new();
    }
    let sqrt = num.integer_sqrt();
    let mut result = vec![1];
    for factor in 2..=sqrt {
        if num % factor != 0 {
            continue;
        }
        result.push(factor);
        let other_factor = num / factor;
        if other_factor != factor {
            result.push(other_factor);
        }
    }
    result
}
