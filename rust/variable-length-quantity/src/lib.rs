const MASK: u32 = (1 << 8) - 1;
const MASK_LENGTH: usize = 7;
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
    let mut bytes = Vec::new();
    let mut value = *value;
    while value > 0 {
        let (byte, rest) = to_byte(&value);
        bytes.push(byte);
        value = rest;
    }
    bytes
}

fn to_byte(value: &u32) -> (u8, u32) {
    let byte = (value & MASK) as u8;
    let rest = value >> MASK_LENGTH;
    if rest > 0 {
        (byte | CONTINUE_BIT, rest)
    } else {
        (byte, rest)
    }
}
