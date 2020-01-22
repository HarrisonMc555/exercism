#[macro_use]
extern crate lazy_static;

use regex::Regex;
use std::cmp;
use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Mul, Sub};

const BASE_U32: u32 = 10;
const BASE_U8: u8 = 10;

/// Type implementing arbitrary-precision decimal arithmetic
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Decimal {
    sign: Sign,
    exponent: i32,
    digits: Vec<u8>,
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
enum Sign {
    Negative,
    Positive,
}

impl Decimal {
    pub fn zero() -> Self {
        Decimal {
            digits: vec![0],
            exponent: 0,
            sign: Sign::Positive,
        }
    }

    pub fn is_zero(&self) -> bool {
        self == &Decimal::zero()
    }

    pub fn try_from(input: &str) -> Option<Self> {
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"(\+|-)?(\d+)(?:\.(\d+))?").unwrap();
        }
        let caps = RE.captures(input)?;
        let sign = if let Some(mat) = caps.get(1) {
            match mat.as_str() {
                "+" => Sign::Positive,
                "-" => Sign::Negative,
                _ => unreachable!(),
            }
        } else {
            Sign::Positive
        };
        let mut digits = to_digits(caps.get(2).unwrap().as_str()).unwrap();
        let exponent = (digits.len() - 1) as i32;
        if let Some(mat) = caps.get(3) {
            digits.extend(to_digits(mat.as_str()).unwrap());
        }
        Some(Decimal::cleaned(digits, exponent, sign))
    }

    pub fn get_digit(&self, exponent_index: i32) -> u8 {
        let adjusted_index = self.exponent - exponent_index;
        self.digits
            .get(adjusted_index as usize)
            .cloned()
            .unwrap_or(0)
    }

    fn multiply_by_digit(&mut self, multiplicand: u8) {
        let mut carry = 0;
        for digit in self.digits.iter_mut() {
            let result = *digit * multiplicand;
            *digit = result % BASE_U8;
            carry = result / BASE_U8;
        }
        while carry > 0 {
            let new_digit = carry % BASE_U8;
            carry /= BASE_U8;
            self.exponent += 1;
            self.digits.insert(0, new_digit);
        }
    }

    fn cleaned(digits: Vec<u8>, exponent: i32, sign: Sign) -> Self {
        let mut d = Decimal {
            digits,
            exponent,
            sign,
        };
        d.clean();
        d
    }

    fn clean(&mut self) {
        self.strip_leading_zeros();
        self.strip_trailing_zeros();
    }

    fn strip_leading_zeros(&mut self) {
        let num_leading_zeros =
            self.digits.iter().take_while(|&&digit| digit == 0).count();
        let num_digits_to_keep = self.digits.len() - num_leading_zeros;
        if num_digits_to_keep == 0 {
            *self = Decimal::zero();
            return;
        }
        self.digits.drain(..num_leading_zeros);
        self.exponent -= num_leading_zeros as i32;
    }

    fn strip_trailing_zeros(&mut self) {
        let num_trailing_zeros = self
            .digits
            .iter()
            .rev()
            .take_while(|&&digit| digit == 0)
            .count();
        let num_digits_to_keep = self.digits.len() - num_trailing_zeros;
        if num_digits_to_keep == 0 {
            *self = Decimal::zero();
            return;
        }
        self.digits.truncate(num_digits_to_keep);
    }

    fn min_exponent(&self) -> i32 {
        self.exponent - self.digits.len() as i32 + 1
    }

    fn max_exponent(&self) -> i32 {
        self.exponent
    }

    fn add_exponent(&mut self, exponent: i32) {
        self.exponent += exponent;
    }

    fn negated(&self) -> Decimal {
        Decimal {
            sign: self.sign.negated(),
            ..self.clone()
        }
    }

    fn add(&self, other: &Decimal) -> Decimal {
        match (self.sign, other.sign) {
            (Sign::Positive, Sign::Positive) => (),
            (Sign::Positive, Sign::Negative) => {
                return self.sub(&other.negated())
            }
            (Sign::Negative, Sign::Positive) => {
                return other.sub(&self.negated())
            }
            (Sign::Negative, Sign::Negative) => (),
        }

        let min_exponent = cmp::min(self.min_exponent(), other.min_exponent());
        let max_exponent = cmp::max(self.max_exponent(), other.max_exponent());
        let num_digits = (max_exponent - min_exponent + 1) as usize;
        let mut digits = vec![0; num_digits];

        let mut carry: u8 = 0;
        for exponent in min_exponent..=max_exponent {
            let digit_total =
                self.get_digit(exponent) + other.get_digit(exponent) + carry;
            let added_digit = digit_total % BASE_U8;
            carry = digit_total / BASE_U8;
            let index = (max_exponent - exponent) as usize;
            digits[index] = added_digit;
        }

        let exponent = if carry == 0 {
            max_exponent
        } else {
            assert!(carry < BASE_U8);
            digits.insert(0, carry);
            max_exponent + 1
        };

        Decimal::cleaned(digits, exponent, self.sign)
    }

    fn sub(&self, other: &Decimal) -> Decimal {
        match (self.sign, other.sign) {
            (Sign::Positive, Sign::Positive) => (),
            (Sign::Positive, Sign::Negative) => {
                return self.add(&other.negated())
            }
            (Sign::Negative, Sign::Positive) => {
                return other.add(&self.negated()).negated()
            }
            (Sign::Negative, Sign::Negative) => {
                return other.negated().sub(self.negated());
            }
        }

        match (self.is_zero(), other.is_zero()) {
            (true, true) => return Decimal::zero(),
            (true, false) => return other.negated(),
            (false, true) => return self.clone(),
            _ => (),
        }

        if self < other {
            return other.sub(&self).negated();
        }

        let min_exponent = cmp::min(self.min_exponent(), other.min_exponent());
        let max_exponent = cmp::max(self.max_exponent(), other.max_exponent());
        let num_digits = (max_exponent - min_exponent + 1) as usize;
        let mut digits = vec![0; num_digits];

        let mut borrow: u8 = 0;

        for exponent in min_exponent..=max_exponent {
            let to_subtract = other.get_digit(exponent) + borrow;
            let mut self_digit = self.get_digit(exponent);
            borrow = if to_subtract > self_digit {
                self_digit += BASE_U8;
                1
            } else {
                0
            };
            let difference = self_digit - to_subtract;
            let index = (max_exponent - exponent) as usize;
            digits[index] = difference;
        }

        if borrow > 0 {
            panic!("Attempted to subtract with underflow");
        }

        let exponent = max_exponent;

        Decimal::cleaned(digits, exponent, self.sign)
    }

    fn mul(&self, other: &Decimal) -> Decimal {
        let positive_result = other.digits.iter().enumerate().fold(
            Decimal::zero(),
            |result, (offset, &digit)| {
                let exponent = other.exponent - offset as i32;
                let mut copy = self.clone();
                copy.add_exponent(exponent);
                copy.multiply_by_digit(digit);
                result + copy
            },
        );
        let sign = if self.sign == other.sign {
            Sign::Positive
        } else {
            Sign::Negative
        };
        Decimal {
            sign,
            ..positive_result
        }
    }
}

impl<'a, 'b> Add<&'b Decimal> for &'a Decimal {
    type Output = Decimal;

    fn add(self, other: &'b Decimal) -> Decimal {
        self.add(other)
    }
}

impl Add for Decimal {
    type Output = Decimal;

    fn add(self, other: Decimal) -> Decimal {
        (&self).add(&other)
    }
}

impl AddAssign for Decimal {
    fn add_assign(&mut self, rhs: Decimal) {
        let lhs: &Decimal = self;
        let result = lhs.add(&rhs);
        *self = result;
    }
}

impl<'a, 'b> Sub<&'b Decimal> for &'a Decimal {
    type Output = Decimal;

    fn sub(self, other: &'b Decimal) -> Decimal {
        self.sub(other)
    }
}

impl Sub for Decimal {
    type Output = Decimal;

    fn sub(self, other: Decimal) -> Decimal {
        (&self).sub(&other)
    }
}

impl Mul for Decimal {
    type Output = Decimal;

    fn mul(self, other: Decimal) -> Decimal {
        (&self).mul(&other)
    }
}

impl<'a, 'b> Mul<&'b Decimal> for &'a Decimal {
    type Output = Decimal;

    fn mul(self, other: &'b Decimal) -> Decimal {
        self.mul(other)
    }
}

impl PartialOrd for Decimal {
    fn partial_cmp(&self, other: &Decimal) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Decimal {
    fn cmp(&self, other: &Decimal) -> Ordering {
        match (self.sign, other.sign) {
            (Sign::Positive, Sign::Positive) => (),
            (Sign::Positive, Sign::Negative) => return Ordering::Greater,
            (Sign::Negative, Sign::Positive) => return Ordering::Less,
            (Sign::Negative, Sign::Negative) => {
                return other.negated().cmp(&self.negated())
            }
        }
        // Now we know that both are positive
        match (self.is_zero(), other.is_zero()) {
            (true, true) => return Ordering::Equal,
            (true, false) => return Ordering::Less,
            (false, true) => return Ordering::Greater,
            (false, false) => (),
        }
        if self.exponent != other.exponent {
            return self.exponent.cmp(&other.exponent);
        }
        self.digits.cmp(&other.digits)
    }
}

impl Sign {
    pub fn negated(self) -> Sign {
        match self {
            Sign::Positive => Sign::Negative,
            Sign::Negative => Sign::Positive,
        }
    }
}

fn to_digits(string: &str) -> Option<Vec<u8>> {
    string
        .chars()
        .map(|c| c.to_digit(BASE_U32).map(|d| d as u8))
        .collect::<Option<Vec<_>>>()
}
