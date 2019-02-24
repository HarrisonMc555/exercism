use std::cmp;
// use std::ops::{Add, AddAssign, Mul, Sub};
use std::ops::{Add, AddAssign, Mul, Sub};

const BASE_32_BITS: u32 = 10;
const BASE_8_BITS: u8 = 10;

/// Type implementing arbitrary-precision decimal arithmetic
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Decimal {
    digits: Vec<u8>,
    exponent: i32,
}

impl Decimal {
    pub fn zero() -> Self {
        Decimal {
            digits: vec![0],
            exponent: 0,
        }
    }

    pub fn from_digit(digit: u8) -> Self {
        Decimal {
            digits: vec![digit],
            exponent: 0,
        }
    }

    pub fn try_from(input: &str) -> Option<Self> {
        let mut digits = Vec::new();
        let mut maybe_exponent = None;
        for (index, c) in input.chars().enumerate() {
            match c {
                '0'..='9' => digits.push(c.to_digit(BASE_32_BITS)? as u8),
                '.' if maybe_exponent.is_none() => {
                    maybe_exponent = Some(index as i32 - 1);
                }
                _ => return None,
            }
        }
        let exponent = maybe_exponent.unwrap_or(digits.len() as i32);
        Some(Decimal::cleaned(digits, exponent))
    }

    pub fn get_digit(&self, exponent_index: i32) -> u8 {
        let adjusted_index = self.exponent - exponent_index;
        if adjusted_index < 0 {
            return 0;
        }
        self.digits
            .get(adjusted_index as usize)
            .cloned()
            .unwrap_or(0)
    }

    fn cleaned(digits: Vec<u8>, exponent: i32) -> Self {
        let mut d = Decimal { digits, exponent };
        println!("Before stripping: {:?}", d);
        d.clean();
        println!("After stripping: {:?}", d);
        d
    }

    fn clean(&mut self) {
        self.strip_trailing_zeros();
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
}

impl<'a, 'b> Add<&'b Decimal> for &'a Decimal {
    type Output = Decimal;

    fn add(self, other: &'b Decimal) -> Decimal {
        let min_exponent = cmp::min(self.min_exponent(), other.min_exponent());
        let max_exponent = cmp::max(self.max_exponent(), other.max_exponent());
        let num_digits = (max_exponent - min_exponent + 1) as usize;
        let mut digits = vec![0; num_digits];

        let mut carry: u8 = 0;
        for exponent in min_exponent..=max_exponent {
            let digit_total =
                self.get_digit(exponent) + other.get_digit(exponent) + carry;
            let added_digit = digit_total % BASE_8_BITS;
            carry = digit_total / BASE_8_BITS;
            let index = (max_exponent - exponent) as usize;
            digits[index] = added_digit;
        }

        let exponent = if carry == 0 {
            max_exponent
        } else {
            assert!(carry < BASE_8_BITS);
            digits.insert(0, carry);
            max_exponent + 1
        };

        Decimal::cleaned(digits, exponent)
    }
}

impl Add for Decimal {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        &self + &other
    }
}

impl AddAssign for Decimal {
    fn add_assign(&mut self, rhs: Decimal) {
        let lhs: &Decimal = self;
        let result = lhs + &rhs;
        *self = result;
    }
}

impl<'a, 'b> Sub<&'b Decimal> for &'a Decimal {
    type Output = Decimal;

    fn sub(self, other: &'b Decimal) -> Decimal {
        println!("sub: {:?} - {:?}", self, other);

        let min_exponent = cmp::min(self.min_exponent(), other.min_exponent());
        let max_exponent = cmp::max(self.max_exponent(), other.max_exponent());
        let num_digits = (max_exponent - min_exponent + 1) as usize;
        let mut digits = vec![0; num_digits];

        let mut borrow: u8 = 0;

        for exponent in (min_exponent..=max_exponent).rev() {
            let to_subtract = other.get_digit(exponent) + borrow;
            let mut self_digit = self.get_digit(exponent);
            if to_subtract > self_digit {
                borrow = 1;
                self_digit += BASE_8_BITS;
            } else {
                borrow = 0;
            }
            let difference = self_digit - to_subtract;
            let index = (max_exponent - exponent) as usize;
            digits[index] = difference;
        }

        if borrow > 0 {
            panic!("Attempted to subtract with underflow");
        }

        let exponent = max_exponent;

        Decimal::cleaned(digits, exponent)
    }
}

impl Sub for Decimal {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        &self - &other
    }
}

// impl Mul for Decimal {
//     type Output = Self;

//     fn mul(self, other: Self) -> Self {
//         let mut result = Decimal::zero();
//         let mut carry: u8 = 0;

//         for (offset, &digit) in other.trailing.iter().enumerate().rev() {
//             let mut copy = self.clone();
//             let shift_amount = offset + 1;
//             copy.shift_right(shift_amount);
//             copy.multiply_by_digit(digit);
//             result += copy;
//         }

//         // for (offset, &digit) in

//         result
//     }
// }
