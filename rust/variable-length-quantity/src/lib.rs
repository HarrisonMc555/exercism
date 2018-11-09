const MASK_LENGTH: usize = 7;
const MASK: u32 = (1 << MASK_LENGTH) - 1;
const CONTINUE_BIT: u8 = 1 << MASK_LENGTH;
const MAX_BYTES: usize = 5;
const HIGHEST_BYTE_MAX_VALUE: u8 = 0x0f;

#[derive(Debug, PartialEq)]
pub enum Error {
    IncompleteNumber,
    Overflow,
}

/// Convert a list of numbers to a stream of bytes encoded with variable length
/// encoding.
pub fn to_bytes(values: &[u32]) -> Vec<u8> {
    values.iter().flat_map(|x| value_to_bytes(x)).collect()
}

/// Given a stream of bytes, extract all numbers which are encoded in there.
pub fn from_bytes(bytes: &[u8]) -> Result<Vec<u32>, Error> {
    let mut result = Vec::new();
    let mut bytes = bytes;
    while !bytes.is_empty() {
        let value_bytes = get_value_bytes(bytes)?;
        result.push(decode_value(value_bytes));
        bytes = &bytes[value_bytes.len()..];
    }
    Ok(result)
}

///////////////////////////////////////////////////////////////////////////////
// Encoding helper functions
///////////////////////////////////////////////////////////////////////////////

fn value_to_bytes(value: &u32) -> Vec<u8> {
    let (last, mut rest) = encode_last_byte(value);
    let mut bytes = vec![last];
    while rest > 0 {
        let (byte, next_rest) = encode_other_byte(&rest);
        bytes.push(byte);
        rest = next_rest;
    }
    bytes.reverse();
    bytes
}

fn encode_last_byte(value: &u32) -> (u8, u32) {
    extract_bits(value)
}

fn encode_other_byte(value: &u32) -> (u8, u32) {
    let (byte, rest) = extract_bits(value);
    (byte | CONTINUE_BIT, rest)
}

fn extract_bits(value: &u32) -> (u8, u32) {
    let byte = (value & MASK) as u8;
    let rest = value >> MASK_LENGTH;
    (byte, rest)
}

///////////////////////////////////////////////////////////////////////////////
// Decoding helper functions
///////////////////////////////////////////////////////////////////////////////

fn decode_value(bytes: &[u8]) -> u32 {
    let last_index = bytes.len() - 1;
    let other_bytes = &bytes[..last_index];
    let last_byte = bytes[last_index];
    let mut result: u32 = 0;
    for byte in other_bytes {
        result <<= MASK_LENGTH;
        result |= decode_other_byte(byte);
    }
    result <<= MASK_LENGTH;
    result | decode_last_byte(&last_byte)
}

fn get_value_bytes(bytes: &[u8]) -> Result<&[u8], Error> {
    let length = first_value_length(bytes)?;
    let bytes = &bytes[..length];
    if is_overflow(bytes) {
        Err(Error::Overflow)
    } else {
        Ok(bytes)
    }
}

fn is_last_byte(byte: &u8) -> bool {
    byte & CONTINUE_BIT == 0
}

#[allow(dead_code)]
fn is_other_byte(byte: &u8) -> bool {
    !is_last_byte(byte)
}

fn decode_last_byte(byte: &u8) -> u32 {
    *byte as u32
}

fn decode_other_byte(byte: &u8) -> u32 {
    (byte ^ CONTINUE_BIT) as u32
}

fn first_value_length(bytes: &[u8]) -> Result<usize, Error> {
    bytes
        .iter()
        .take(MAX_BYTES)
        .position(|byte| is_last_byte(byte))
        .map(|index| index + 1)
        .ok_or(Error::IncompleteNumber)
}

fn is_overflow(bytes: &[u8]) -> bool {
    bytes.len() == MAX_BYTES
        && decode_other_byte(&bytes[0]) > HIGHEST_BYTE_MAX_VALUE as u32
}
