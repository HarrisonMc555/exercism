#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidInputBase,
    InvalidOutputBase,
    InvalidDigit(u32),
}

///
/// Convert a number between two bases.
///
/// A number is any slice of digits.
/// A digit is any unsigned integer (e.g. u8, u16, u32, u64, or usize).
/// Bases are specified as unsigned integers.
///
/// Return an `Err(.)` if the conversion is impossible.
/// The tests do not test for specific values inside the `Err(.)`.
///
///
/// You are allowed to change the function signature as long as all test still pass.
///
///
/// Example:
/// Input
///   number: &[4, 2]
///   from_base: 10
///   to_base: 2
/// Result
///   Ok(vec![1, 0, 1, 0, 1, 0])
///
/// The example corresponds to converting the number 42 from decimal
/// which is equivalent to 101010 in binary.
///
///
/// Notes:
///  * The empty slice ( "[]" ) is equal to the number 0.
///  * Never output leading 0 digits. However, your function must be able to
///     process input with leading 0 digits.
///
pub fn convert(number: &[u32], from_base: u32, to_base: u32) -> Result<Vec<u32>, Error> {
    let native_number = from_digits(number, from_base)?;
    to_digits(native_number, to_base)
}

fn from_digits(digits: &[u32], base: u32) -> Result<u32, Error> {
    if base <= 1 {
        return Err(Error::InvalidInputBase);
    }
    let mut result = 0;
    let mut power = 1;
    for (index, &digit) in digits.iter().rev().enumerate() {
        if digit >= base {
            let index = digits.len() - index;
            return Err(Error::InvalidDigit(index as u32));
        }
        result += digit * power;
        power *= base;
    }
    Ok(result)
}

fn to_digits(number: u32, base: u32) -> Result<Vec<u32>, Error> {
    if base <= 1 {
        return Err(Error::InvalidOutputBase);
    }
    let mut result = Vec::new();
    let mut number = number;
    while number > 0 {
        let digit = number % base;
        eprintln!("number = {}, next digit = {}", number, digit);
        result.push(digit);
        number /= base;
    }
    result.reverse();
    Ok(result)
}
