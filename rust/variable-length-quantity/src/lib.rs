const MASK_LENGTH: usize = 7;
const MASK: u32 = (1 << MASK_LENGTH) - 1;
const CONTINUE_BIT: u8 = 1 << MASK_LENGTH;

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
    unimplemented!("Convert the list of bytes {:?} to a list of numbers", bytes)
}

fn u32_to_bytes(value: &u32) -> Vec<u8> {
    if value == &0 {
        return vec![0];
    }
    let (first, rest) = first_byte(value);
    let mut rest = rest;
    let mut bytes = vec![first];
    while rest > 0 {
        let (byte, next_rest) = other_byte(&rest);
        bytes.push(byte);
        rest = next_rest;
    }
    bytes.iter().rev().cloned().collect()
}

fn first_byte(value: &u32) -> (u8, u32) {
    let byte = (value & MASK) as u8;
    let rest = value >> MASK_LENGTH;
    println!("first_byte({})", value);
    println!("\tbyte: {}", byte);
    println!("\trest: {}", rest);
    (byte, rest)
}

fn other_byte(value: &u32) -> (u8, u32) {
    let byte = (value & MASK) as u8;
    let rest = value >> MASK_LENGTH;
    println!("other_byte({})", value);
    println!("\tbyte: {}", byte);
    println!("\trest: {}", rest);
    (byte | CONTINUE_BIT, rest)
}
