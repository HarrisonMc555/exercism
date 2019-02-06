use std::cmp;
use std::ops::{Add, Sub};

const BASE_32_BITS: u32 = 10;
const BASE_8_BITS: u8 = 10;

/// Type implementing arbitrary-precision decimal arithmetic
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Decimal {
    leading: Vec<u8>,
    trailing: Vec<u8>,
}

impl Decimal {
    pub fn try_from(input: &str) -> Option<Decimal> {
        let mut leading = Vec::new();
        let mut chars = input.chars();
        loop {
            match chars.next() {
                Some(c) => match c {
                    '0'..='9' => leading.push(c.to_digit(BASE_32_BITS)? as u8),
                    '.' => break,
                    _ => return None,
                },
                None => {
                    let mut d = Decimal {
                        leading,
                        trailing: Vec::new(),
                    };
                    println!("Before stripping: d: {:?}", d);
                    d.strip_leading_trailing_zeros();
                    println!("After stripping: d: {:?}", d);
                    return Some(d);
                }
            }
        }

        let mut trailing = Vec::new();
        for c in chars {
            if !c.is_ascii_digit() {
                return None;
            } else {
                trailing.push(c.to_digit(BASE_32_BITS)? as u8);
            }
        }
        let mut d = Decimal { leading, trailing };
        println!("Before stripping: d: {:?}", d);
        d.strip_leading_trailing_zeros();
        println!("After stripping: d: {:?}", d);
        Some(d)
    }

    fn strip_leading_trailing_zeros(&mut self) {
        if self.leading.is_empty() {
            self.leading = vec![0];
        }
        let num_leading_zeros =
            self.leading.iter().take_while(|&&digit| digit == 0).count();
        let num_leading_to_strip =
            cmp::min(num_leading_zeros, self.leading.len() - 1);
        self.leading.drain(0..num_leading_to_strip);

        if self.trailing.is_empty() {
            self.trailing = vec![0];
        }
        let num_trailing_zeros = self
            .trailing
            .iter()
            .rev()
            .take_while(|&&digit| digit == 0)
            .count();
        let num_trailing_to_strip =
            cmp::min(num_trailing_zeros, self.trailing.len() - 1);
        self.trailing.drain(0..num_trailing_to_strip);
    }
}

impl Add for Decimal {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let mut carry: u8 = 0;

        let mut trailing = Vec::new();
        for (digit1, digit2) in
            self.trailing.iter().rev().zip(other.trailing.iter().rev())
        {
            let total = digit1 + digit2 + carry;
            let added_digit = total % BASE_8_BITS;
            carry = total / BASE_8_BITS;
            trailing.push(added_digit);
        }
        trailing.reverse();

        let mut leading = Vec::new();
        for (digit1, digit2) in
            self.leading.iter().rev().zip(other.leading.iter().rev())
        {
            let total = digit1 + digit2 + carry;
            let added_digit = total % BASE_8_BITS;
            carry = total / BASE_8_BITS;
            leading.push(added_digit);
        }
        leading.reverse();

        let mut d = Decimal { leading, trailing };
        d.strip_leading_trailing_zeros();
        d
    }
}

impl Sub for Decimal {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        println!("sub: {:?} - {:?}", self, other);

        let mut borrow: u8 = 0;

        let mut trailing = Vec::new();
        for (digit1, digit2) in
            self.trailing.iter().rev().zip(other.trailing.iter().rev())
        {
            let mut digit1 = *digit1;
            print!("{} (- {}) - {}", digit1, borrow, digit2);
            let to_subtract = digit2 + borrow;
            if to_subtract > digit1 {
                borrow = 1;
                digit1 += BASE_8_BITS;
            } else {
                borrow = 0;
            };
            let difference = digit1 - to_subtract;
            println!(" = {} (borrow {})", difference, borrow);
            trailing.push(difference);
        }
        trailing.reverse();

        let mut leading = Vec::new();
        for (digit1, digit2) in
            self.leading.iter().rev().zip(other.leading.iter().rev())
        {
            print!("{} (- {}) - {}", digit1, borrow, digit2);
            let mut digit1 = *digit1;
            let to_subtract = digit2 + borrow;
            if to_subtract > digit1 {
                borrow = 1;
                digit1 += BASE_8_BITS;
            } else {
                borrow = 0;
            };
            let difference = digit1 - to_subtract;
            println!(" = {} (borrow {})", difference, borrow);
            leading.push(difference);
        }
        leading.reverse();

        let mut d = Decimal { leading, trailing };
        d.strip_leading_trailing_zeros();
        d
    }
    
}
