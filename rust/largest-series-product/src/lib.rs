//! Largest product in a series
//! 
//! The [`lsp`] function returns the largest product in a series of base 10
//! digits.
//! 
//! The [`Error`] enum is an error type returned by [`lsp`] if it is used in an
//! invalid way.
//! 
//! [`lsp`]: fn.lsp.html
//! [`Error`]: enum.Error.html

/// An error type for [`largest_series_product`].
///
/// Returned when the [`lsp`] function is used in an invalid way.
///
/// [`largest_series_product`]: index.html
/// [`lsp`]: fn.lsp.html
#[derive(Debug, PartialEq)]
pub enum Error {
    /// The span is longer than the input
    SpanTooLong,
    /// There is an invalid ascii base 10 digit in the input
    InvalidDigit(char),
}

/// Find the largest series product in a string of digits
///
/// Each character in the string is treated as a single, base 10 digit. Each
/// successive slice (with overlap) of `span` digits are multiplied
/// together. The largest product is returned.
///
/// # Errors
///
/// A [`SpanTooLong`] error is returned if `span` is longer than the length of
/// `string_digits`.
///
/// An [`InvalidDigit`] error is returned if any of the characters are not valid
/// ascii base 10 digits. It will contain the first encountered invalid ascii
/// base 10 digit.
///
/// # Examples
///
/// ```
/// # use largest_series_product::{lsp, Error};
/// assert_eq!(lsp("123", 2), Ok(6));
/// assert_eq!(lsp("423", 2), Ok(8));
/// assert_eq!(lsp("123", 7), Err(Error::SpanTooLong));
/// assert_eq!(lsp("AB3", 2), Err(Error::InvalidDigit('A')));
/// ```
/// [`SpanTooLong`]: enum.Error.html#variant.SpanTooLong
/// [`InvalidDigit`]: enum.Error.html#variant.InvalidDigit
pub fn lsp(string_digits: &str, span: usize) -> Result<u64, Error> {
    if span == 0 {
        return Ok(1);
    }
    if span > string_digits.len() {
        return Err(Error::SpanTooLong);
    }
    let digits = to_digits(string_digits, RADIX)?;
    Ok(digits
        .windows(span)
        .map(|window| window.into_iter().product())
        .max()
        .unwrap_or(1))
}

const RADIX: u32 = 10;

fn to_digits(string_digits: &str, radix: u32) -> Result<Vec<u64>, Error> {
    string_digits.chars()
        .map(|c| c
             .to_digit(radix)
             .ok_or(Error::InvalidDigit(c))
             .map(|d| u64::from(d)))
        .collect()
}
