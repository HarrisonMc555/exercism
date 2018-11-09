const MASK_LENGTH: usize = 7;
const MASK: u32 = (1 << MASK_LENGTH) - 1;
const CONTINUE_BIT: u8 = 1 << MASK_LENGTH;
const MAX_BYTES: usize = 5;

#[derive(Debug, PartialEq)]
pub enum Error {
    IncompleteNumber,
    Overflow,
}

/// Convert a list of numbers to a stream of bytes encoded with variable length
/// encoding.
pub fn to_bytes(values: &[u32]) -> Vec<u8> {
    values.iter().flat_map(|x| u32_to_bytes(x)).collect()
}

/// Given a stream of bytes, extract all numbers which are encoded in there.
pub fn from_bytes(bytes: &[u8]) -> Result<Vec<u32>, Error> {
    let mut result = Vec::new();
    let mut bytes = bytes;
    let mut value_bytes = get_value_bytes(bytes)?;
    while !value_bytes.is_empty() {
        result.extend(decode_value(value_bytes));
        bytes = &bytes[value_bytes.len()..];
        value_bytes = get_value_bytes(bytes)?;
    }
    Ok(result)
}

fn u32_to_bytes(value: &u32) -> Vec<u8> {
    if value == &0 {
        return vec![0];
    }
    let (first, rest) = encode_first_byte(value);
    let mut rest = rest;
    let mut bytes = vec![first];
    while rest > 0 {
        let (byte, next_rest) = encode_other_byte(&rest);
        bytes.push(byte);
        rest = next_rest;
    }
    bytes.reverse();
    bytes
}

fn encode_first_byte(value: &u32) -> (u8, u32) {
    let byte = (value & MASK) as u8;
    let rest = value >> MASK_LENGTH;
    (byte, rest)
}

fn encode_other_byte(value: &u32) -> (u8, u32) {
    let byte = (value & MASK) as u8;
    let rest = value >> MASK_LENGTH;
    (byte | CONTINUE_BIT, rest)
}

fn decode_value(bytes: &[u8]) -> Result<u32, Error> {
    let other_bytes = &bytes[..bytes.len() - 1];
    let first_byte = bytes[bytes.len() - 1];
    let mut result: u32 = 0;
    for byte in other_bytes {
        result <<= MASK_LENGTH;
        result |= decode_other_byte(byte);
    }
    result <<= MASK_LENGTH;
    result |= decode_last_byte(&first_byte);
    Ok(result)
}

fn get_value_bytes(bytes: &[u8]) -> Result<&[u8], Error> {
    println!("get_value_bytes({:?})", bytes);
    if bytes.is_empty() {
        return Ok(&[]);
    }
    if let Some(index) = bytes
        .iter()
        .take(MAX_BYTES)
        .position(|byte| is_last_byte(byte))
    {
        println!("\tIndex of last byte is {}", index);
        Ok(&bytes[..index + 1])
    } else {
        println!("\tNo last byte found");
        Err(Error::IncompleteNumber)
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
    (byte & !CONTINUE_BIT) as u32
}
